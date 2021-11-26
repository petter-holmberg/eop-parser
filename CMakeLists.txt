cmake_minimum_required (VERSION 3.16)

project (EopParser)

include_directories ("${PROJECT_SOURCE_DIR}/include")
set (CMAKE_BUILD_TYPE Release)
set (CMAKE_CXX_COMPILER clang++)

add_executable (eop_parser src/main.cpp src/eop_parser.cpp src/eop_ast.cpp)
target_compile_options (
    eop_parser
    PUBLIC
    -std=c++20
    -O3
    -Wall
    -Werror
    -Wextra
    -Wshadow
    -Wnon-virtual-dtor
    -Wno-old-style-cast
    -Wcast-align
    -Wunused
    -Woverloaded-virtual
    -Wpedantic
    -Wconversion
    -Wsign-conversion
    -Wdouble-promotion
    -Wformat=2
    -Wno-unused-variable
    -Wno-ignored-attributes
    -Wno-return-type
    -pedantic
)
