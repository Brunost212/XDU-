cmake_minimum_required(VERSION 3.16)
project(CfgMapPlugin LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# 1) 告诉 CMake 在哪找 LLVM（用 llvm-config --cmakedir 的结果）
set(LLVM_DIR "/usr/lib/llvm-18/lib/cmake/llvm")
find_package(LLVM REQUIRED CONFIG)

# 2) 告诉 CMake 在哪找 Clang
set(CLANG_DIR "/usr/lib/llvm-18/lib/cmake/clang")
find_package(Clang REQUIRED CONFIG
    PATHS ${CLANG_DIR}
    NO_DEFAULT_PATH)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Using ClangConfig.cmake in: ${CLANG_DIR}")

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${CLANG_INCLUDE_DIRS})
add_definitions(${LLVM_DEFINITIONS})

# 3) 真正的插件目标：做一个 MODULE 类型的 shared library
add_library(CfgMapPlugin MODULE
    CfgMapPlugin.cpp
)

# 4) 链接所需的 clang/llvm 组件（保守一点，多连几个）
target_link_libraries(CfgMapPlugin
    PRIVATE
    clangAST
    clangAnalysis
    clangBasic
    clangFrontend
    clangSerialization
)

# 5) 避免在名字前面加 "lib" 也无所谓，clang -load 两种都能识别
# 如果你想强制有 lib 前缀，可以注释掉下面这一行
set_target_properties(CfgMapPlugin PROPERTIES PREFIX "")

