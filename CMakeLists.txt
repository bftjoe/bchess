
cmake_minimum_required(VERSION 3.16)
project(bchess LANGUAGES CXX)

# Collect sources: root-level bchess.cpp and any src/*.cpp
set(SRCS
	${CMAKE_SOURCE_DIR}/bchess.cpp
)
file(GLOB SRC_DIR_SRCS "${CMAKE_SOURCE_DIR}/src/*.cpp")
if(SRC_DIR_SRCS)
	list(APPEND SRCS ${SRC_DIR_SRCS})
endif()

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# Place built executables in the ./build directory (same as the Makefile)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_SOURCE_DIR}/build")

option(USE_CLANG "Try to use clang++ as the C++ compiler" ON)
if(USE_CLANG)
	find_program(CLANGXX clang++)
	if(CLANGXX)
		message(STATUS "Forcing C++ compiler to clang++: ${CLANGXX}")
		set(CMAKE_CXX_COMPILER "${CLANGXX}" CACHE FILEPATH "C++ compiler" FORCE)
	else()
		message(WARNING "clang++ not found; continuing with default C++ compiler")
	endif()
endif()


add_executable(bchess ${SRCS})

# Match Makefile naming: create config-specific output names
set_target_properties(bchess PROPERTIES
	OUTPUT_NAME_DEBUG "bchess-debug"
	OUTPUT_NAME_RELEASE "bchess-release"
	OUTPUT_NAME_RELWITHDEBINFO "bchess-release"
)

# Include paths from the Makefile
target_include_directories(bchess PRIVATE ${CMAKE_SOURCE_DIR}/gtl/include)

# Common compile options for GCC/Clang (Makefile used -Wall -fno-rtti -march=native -mtune=native)
 # Apply options only for Clang/GNU toolchains
target_compile_options(bchess PRIVATE
	$<$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:GNU>>:-Wall -fno-rtti -march=native -mtune=native -D_CRT_SECURE_NO_WARNINGS>

	# Debug flags for GCC/Clang
	$<$<AND:$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:GNU>>,$<CONFIG:Debug>>:-g -O0 -DDEBUG>

	# Release flags for GCC/Clang
	$<$<AND:$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:GNU>>,$<CONFIG:Release>>:-O3 -funroll-loops -finline -fomit-frame-pointer -DNDEBUG>
)

# Linker flags: apply -static for GCC/Clang Release builds to mirror LDFLAGS_RELEASE
target_link_options(bchess PRIVATE
	$<$<AND:$<OR:$<CXX_COMPILER_ID:Clang>,$<CXX_COMPILER_ID:GNU>>,$<CONFIG:Release>>:-static>
)

# If MSVC is detected, report a helpful note (Makefile expects clang/gcc on Windows)
if(MSVC)
	message(STATUS "MSVC toolchain detected. The original Makefile targets clang/gcc; consider running CMake with -DUSE_CLANG=ON and a fresh build directory if you want clang builds on Windows.")
endif()

message(STATUS "Project: ${PROJECT_NAME}")
message(STATUS "Sources: ${SRCS}")

# PGO notes:
# The original Makefile had a PGO workflow (instrument, run to generate profiles, merge with llvm-profdata,
# and recompile with -fprofile-instr-use). Automating PGO in CMake is possible with custom targets but
# it couples the build to clang/llvm tools. If you want automated PGO targets (pgo-generate, pgo-use),
# I can add them; they will assume clang/llvm tools are available in PATH.

# ------------------ PGO custom targets (optional) ------------------
set(TARGET_NAME "bchess")
set(PGO_EXEC "profile-bchess")
set(PGO_DATA "bchess.profdata")

find_program(LLVM_PROFDATA_EXEC llvm-profdata HINTS ENV PATH)
if(NOT LLVM_PROFDATA_EXEC)
	message(STATUS "llvm-profdata not found; PGO targets will be unavailable unless llvm-profdata is installed and on PATH")
endif()

# Construct a reasonable release-like flag set used by the Makefile PGO flow
set(PGO_CXX_FLAGS "-Wall -fno-rtti -march=native -mtune=native -I${CMAKE_SOURCE_DIR}/gtl/include -O3 -funroll-loops -finline -fomit-frame-pointer -DNDEBUG")

if(LLVM_PROFDATA_EXEC)
	# pgo-generate: compile instrumented binary and run it to generate .profraw
	add_custom_target(pgo-generate
		COMMAND ${CMAKE_COMMAND} -E echo "Building instrumented binary: ${PGO_EXEC}${CMAKE_EXECUTABLE_SUFFIX}"
		COMMAND ${CMAKE_CXX_COMPILER} ${PGO_CXX_FLAGS} -fprofile-instr-generate -o "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_EXEC}${CMAKE_EXECUTABLE_SUFFIX}" ${SRCS}
		COMMAND ${CMAKE_COMMAND} -E echo "Running instrumented binary to collect profile data..."
		COMMAND "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_EXEC}${CMAKE_EXECUTABLE_SUFFIX}" bench
		COMMAND ${CMAKE_COMMAND} -E echo "Removing instrumented binary"
		COMMAND ${CMAKE_COMMAND} -E remove "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_EXEC}${CMAKE_EXECUTABLE_SUFFIX}"
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
		VERBATIM
	)

	# pgo-merge: merge any produced .profraw files into a single profdata
	add_custom_target(pgo-merge
		COMMAND ${CMAKE_COMMAND} -E echo "Merging .profraw -> ${PGO_DATA}"
		COMMAND ${LLVM_PROFDATA_EXEC} merge -output="${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_DATA}" "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}"/*.profraw
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
		VERBATIM
	)

	# pgo-use: compile final binary using the merged profile
	add_custom_target(pgo-use
		COMMAND ${CMAKE_COMMAND} -E echo "Building PGO-optimized binary: ${TARGET_NAME}-pgo${CMAKE_EXECUTABLE_SUFFIX}"
		COMMAND ${CMAKE_CXX_COMPILER} ${PGO_CXX_FLAGS} -fprofile-instr-use="${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_DATA}" -o "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${TARGET_NAME}-pgo${CMAKE_EXECUTABLE_SUFFIX}" ${SRCS}
		COMMAND ${CMAKE_COMMAND} -E echo "Cleaning up profile raw files and data"
		COMMAND ${CMAKE_COMMAND} -E remove_glob "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/*.profraw"
		COMMAND ${CMAKE_COMMAND} -E remove "${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/${PGO_DATA}"
		WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
		VERBATIM
	)

	# pgo: full pipeline
	add_custom_target(pgo DEPENDS pgo-generate pgo-merge pgo-use)
else()
	add_custom_target(pgo
		COMMAND ${CMAKE_COMMAND} -E echo "PGO targets unavailable: llvm-profdata not found"
	)
endif()

