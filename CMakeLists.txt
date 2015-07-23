cmake_minimum_required ( VERSION 2.8.8 FATAL_ERROR )

# Set CMake language to Fortran
enable_language ( Fortran )
project ( json_fortran_example NONE )

find_package ( jsonfortran-${CMAKE_Fortran_COMPILER_ID} 4.1.1 REQUIRED )
include_directories( "${jsonfortran_INCLUDE_DIRS}" )

# Define string utility module source directory
set ( JSE_SOURCE_DIR ${CMAKE_SOURCE_DIR}/src )
set ( JSE_SOURCE 
  ${JSE_SOURCE_DIR}/main.f90 
  ${JSE_SOURCE_DIR}/myType_type.f90 
)

# Define tests executable
set ( JSE_EXE json-fortran-example )
add_executable ( ${JSE_EXE} ${JSE_SOURCE} )
target_link_libraries ( ${JSE_EXE} jsonfortran-static )
