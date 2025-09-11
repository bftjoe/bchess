#pragma once

#include <chrono>
#include <string>

namespace Belette {

using TimeMs = std::chrono::milliseconds::rep;

inline TimeMs now() {
    return std::chrono::duration_cast<std::chrono::milliseconds> (std::chrono::steady_clock::now().time_since_epoch()).count();
}

inline int parseInt(const std::string &str) {
    try {
        return std::stoi(str);
    } catch (const std::invalid_argument & e) {
        return 0;
    } catch (const std::out_of_range & e) {
        return 0;
    }

    return 0;
}

inline int64_t parseInt64(const std::string &str) {
    try {
        return std::stoll(str);
    } catch (const std::invalid_argument & e) {
        return 0;
    } catch (const std::out_of_range & e) {
        return 0;
    }

    return 0;
}

// https://stackoverflow.com/questions/9779105/generic-member-function-pointer-as-a-template-parameter
template <typename T, typename R, typename ...Args>
class MemberFunctionProxy {
public:
    typedef void (T::*Func)(Args...);
    
    inline MemberFunctionProxy() { };
    inline MemberFunctionProxy(Func func_): func(func_) { };
    inline R operator()(T &obj, Args ...args) { (obj.*func)(std::forward<Args>(args)...); }
private:
    Func func;
};

} /* namespace Belette */

