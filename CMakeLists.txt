cmake_minimum_required(VERSION 3.5)
project(guile_words)

set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

set(SOURCE_FILES
    c/app.c
    scm/words.scm)

add_executable(guile_words ${SOURCE_FILES})