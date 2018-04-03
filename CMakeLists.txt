cmake_minimum_required(VERSION 3.8)
project(number)

set(CMAKE_CXX_STANDARD 14)

set(SOURCE_FILES src/main.cpp src/Number.hpp src/NumberMath.h)
add_executable(number ${SOURCE_FILES})