TARGET_NAME := bchess
TARGET_BIN_DIR := ./build
SRC_DIR := ./src
CXX := clang++

TARGET_SUFFIX =
ifeq ($(OS), Windows_NT)
	TARGET_SUFFIX = .exe
endif

SRCS := bchess.cpp

# idea from Stormphrax
PGO_EXEC := profile-bchess
PGO_GENERATE := -fprofile-instr-generate
PGO_DATA := bchess.profdata
PGO_MERGE := llvm-profdata merge -output=$(PGO_DATA) *.profraw
PGO_USE := -fprofile-instr-use=$(PGO_DATA)

CPPFLAGS := -Wall -std=c++20 -fno-rtti -mbmi -mbmi2 -mpopcnt -msse2 -msse3 -msse4.1 -mavx2 -D_CRT_SECURE_NO_WARNINGS
CPPFLAGS_DEBUG := $(CPPFLAGS) -g -O0 -DDEBUG
CPPFLAGS_RELEASE := $(CPPFLAGS) -O3 -funroll-loops -finline -fomit-frame-pointer -DNDEBUG

LDFLAGS := -Wall -std=c++20 -fno-rtti -mbmi -mbmi2 -mpopcnt -msse2 -msse3 -msse4.1 -mavx2
LDFLAGS_DEBUG := $(LDFLAGS)
LDFLAGS_RELEASE := $(LDFLAGS) -static

.PHONY: all debug release profile

all: pgo release

pgo:
# idea from Stormphrax
	$(eval TARGET_EXEC = $(TARGET_NAME)-pgo)
	$(CXX) $(CPPFLAGS_RELEASE) $(LDFLAGS_RELEASE) $(PGO_GENERATE) -o $(TARGET_BIN_DIR)/$(PGO_EXEC)$(TARGET_SUFFIX)  $(SRCS)
	$(TARGET_BIN_DIR)/$(PGO_EXEC)$(TARGET_SUFFIX) bench
	$(RM) $(TARGET_BIN_DIR)/$(PGO_EXEC)$(TARGET_SUFFIX)
	$(PGO_MERGE)
	$(CXX) $(CPPFLAGS_RELEASE) $(LDFLAGS_RELEASE) $(PGO_USE) -o $(TARGET_BIN_DIR)/$(TARGET_EXEC)$(TARGET_SUFFIX)  $(SRCS)
	$(RM) *.profraw $(PGO_DATA)

release:
	$(eval TARGET_EXEC = $(TARGET_NAME)-release)
	$(CXX) $(CPPFLAGS_RELEASE) $(LDFLAGS_RELEASE) -o $(TARGET_BIN_DIR)/$(TARGET_EXEC)$(TARGET_SUFFIX)  $(SRCS)

debug:
	$(eval TARGET_EXEC = $(TARGET_NAME)-debug)
	$(CXX) $(CPPFLAGS_DEBUG) $(LDFLAGS_DEBUG) -o $(TARGET_BIN_DIR)/$(TARGET_EXEC)$(TARGET_SUFFIX)  $(SRCS)
