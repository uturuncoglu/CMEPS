project(CMEPS_share Fortran)
include(ExternalProject)
set(SRCFILES flux_atmocn_mod.F90 glc_elevclass_mod.F90 perf_mod.F90 ufs_const_mod.F90 ufs_kind_mod.F90)
add_library(cmeps_share ${SRCFILES})
target_include_directories (cmeps_share PUBLIC ${CMAKE_CURRENT_SOURCE_DIR} ${ESMF_F90COMPILEPATHS} ${PIO_Fortran_INCLUDE_DIRS})
install(TARGETS cmeps_share
        LIBRARY DESTINATION lib)
# ESMX requires mod files
foreach (SRC ${SRCFILES})
  string(REGEX REPLACE "[.]F90$" ".mod" MOD ${SRC})
  string(TOLOWER "${MOD}" LOWER_MOD)
  install(FILES "${CMAKE_CURRENT_BINARY_DIR}/${LOWER_MOD}" DESTINATION include)
endforeach ()
