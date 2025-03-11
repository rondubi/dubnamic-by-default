cmake_minimum_required(VERSION 3.20)
project(dynamic-by-default)

# Check if BUILDIT_PATH is defined
if(NOT DEFINED BUILDIT_PATH)
    message(FATAL_ERROR "Error: BUILDIT_PATH is required but not provided.")
else()
    # Add BUILDIT_PATH to the cache if it is defined
    set(BUILDIT_PATH ${BUILDIT_PATH} CACHE PATH "Path to BuildIt")
endif()


SET(BUILDIT_PATH "" CACHE PATH "Path to BuildIt")
link_directories(${BUILDIT_PATH}/build)


find_package(Clang REQUIRED CONFIG)


add_executable(
	dynamic-by-default 
	DBD.cpp
	)

target_include_directories(
	dynamic-by-default
	PUBLIC
	${BUILDIT_PATH}/include
	${CLANG_INCLUDE_DIRS}	
	)


target_link_libraries(dynamic-by-default 
	PUBLIC
	clangTooling
	clangBasic
	clangAST
	clangFrontend
	clangParse
	clangDriver
        buildit
	)
		
