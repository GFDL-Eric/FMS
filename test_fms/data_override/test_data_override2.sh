#!/bin/sh

# This is part of the GFDL FMS package. This is a shell script to
# execute tests in the test_fms/data_override directory.

# Ed Hartnett 11/26/19

# Set common test settings.
. ../test_common.sh

# Copy to builddir and rename data files for tests.
cp $top_srcdir/test_fms/data_override/data_table_base data_table
cp $top_srcdir/test_fms/data_override/diag_table_base diag_table
#cp -r $top_srcdir/test_fms/data_override/INPUT $top_builddir/test_fms/data_override/INPUT

# Both tests are skipped in the bats file, so commented out here.
tnum=$( printf "%2.2d" 1 )
sed "s/<test_num>/${tnum}/"  $top_srcdir/test_fms/data_override/input_base.nml > input.nml
run_test test_data_override 2 skip

tnum=$( printf "%2.2d" 2 )
sed "s/<test_num>/${tnum}/"  $top_srcdir/test_fms/data_override/input_base.nml > input.nml
run_test test_data_override 2 skip

tnum=$( printf "%2.2d" 3 )
sed "s/<test_num>/${tnum}/"  $top_srcdir/test_fms/data_override/input_base.nml > input.nml
run_test test_data_override 1 skip

tnum=$( printf "%2.2d" 4 )
sed "s/<test_num>/${tnum}/"  $top_srcdir/test_fms/data_override/input_base.nml > input.nml
run_test test_data_override_0D 1
