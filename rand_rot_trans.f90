! to randomly rotate and translate pentacene, then to make sure its position is proper.

program main
    use molecule_module
    use progress_bar_module
    use rand_init_module
    ! use ieee_arithmetic
    implicit none
    integer(kind=4) :: argc
    character(kind=1,len=256), allocatable :: argv(:)
    integer(kind=4) :: iarg
    integer(kind=4) :: arg_status
    character(kind=1,len=256) :: ifile_name, ofile_name
    type(molecule) :: mol_in, mol_out, mol_tmp
    integer(kind=4), parameter :: write_to_unit = 11
    real(kind=8) :: x_min, x_max, y_min, y_max, z_min, z_max
    real(kind=8) :: tol_min, tol_max
    integer(kind=4) :: write_file_status, read_str_status
    integer(kind=4) :: num_gene
    integer(kind=4) :: tried
    integer(kind=4) :: tot_gene
    real(kind=8) :: time_start, time_end
    logical :: is_translate_to_origin
    integer(kind=4) :: i, j

    ! get command arguments, not argc in C, this is the argc except argv0.
    argc = command_argument_count()
    allocate(argv(0:argc))
    do iarg = 0, argc
        call get_command_argument(iarg, argv(iarg), arg_status)
    end do

    ! set default arguments
    ifile_name = "pentacene.xyz"
    ofile_name = "traj.xyz"
    x_min = 3.0D0
    x_max = 5.0D0
    y_min = 0.0D0
    y_max = 1.8125D1
    z_min = 0.0D0
    z_max = 9.006D0
    tol_min = 3.0D0
    tol_max = 5.5D0
    tot_gene = 100
    is_translate_to_origin = .false.
    
    ! pharse command arguments
    iarg = 1
    if (argv(iarg) == "--help" .or. argv(iarg) == "-h" .or. argv(iarg) == "/?") then
        write(*, "(a,1x,a)") "Usage:", trim(argv(0))
        write(*, "(4x,a)") "-h | --help | /?       : print this message and exit."
        write(*, "(4x,a)") "--ifile-name IFILE_NAME: input file name."
        write(*, "(4x,a)") "--ofile-name OFILE_NAME: output file name."
        write(*, "(4x,a)") "--x-min      X_MIN     : minimum translation distance along axis x."
        write(*, "(4x,a)") "--x-max      X_MAX     : maximum translation distance along axis x."
        write(*, "(4x,a)") "--y-min      Y_MIN     : minimum translation distance along axis y."
        write(*, "(4x,a)") "--y-max      Y_MAX     : maximum translation distance along axis y."
        write(*, "(4x,a)") "--z-min      Z_MIN     : minimum translation distance along axis z."
        write(*, "(4x,a)") "--z-max      Z_MAX     : maximum translation distance along axis z."
        write(*, "(4x,a)") "--tol-min    TOL_MIN   : minimum tolerance of distance between closest atoms."
        write(*, "(4x,a)") "--tol-max    TOL_MAX   : maximum tolerance of distance between closest atoms."
        write(*, "(4x,a)") "--tot-gene   TOT_GENE  : total amount of molecules in the trajectory."
        write(*, "(4x,a)") "--to-origin  TO_ORIGIN : whether translate the molecule to the origin point."
        stop
    end if
    do while (iarg <= argc)
        if (argv(iarg) == "--ifile-name") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) ifile_name
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--ofile-name") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) ofile_name
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--x-min") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) x_min
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--x-max") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) x_max
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--y-min") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) y_min
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--y-max") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) y_max
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--z-min") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) z_min
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--z-max") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) z_max
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--tol-min") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) tol_min
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--tol-max") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) tol_max
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--tot-gene") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            read(argv(iarg), *, iostat = read_str_status) tot_gene
            if (read_str_status /= 0) then
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else if (argv(iarg) == "--to-origin") then
            iarg = iarg + 1
            if (iarg > argc) then
                write(0, "(a,a,a)") "Error! Cannot get the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
            if (argv(iarg) == "f" .or. argv(iarg) == ".f." .or. argv(iarg) == "false" .or. argv(iarg) == ".false." .or. &
                argv(iarg) == "F" .or. argv(iarg) == ".F." .or. argv(iarg) == "False" .or. argv(iarg) == ".False.") then
                is_translate_to_origin = .false.
            else if (argv(iarg) == "t" .or. argv(iarg) == ".t." .or. argv(iarg) == "true" .or. argv(iarg) == ".true." .or. &
                argv(iarg) == "T" .or. argv(iarg) == ".T." .or. argv(iarg) == "True" .or. argv(iarg) == ".True.") then
                is_translate_to_origin = .true.
            else
                write(0, "(a,a,a)") "Error! Cannot pharse the argument for """, trim(argv(iarg - 1)), """."
                stop 1
            end if
        else
            write(0, "(a,a,a)") "Error! Cannot understand argument """, trim(argv(iarg)), """."
            stop 1
        end if
        iarg = iarg + 1
    end do

    write(*, "(a,1x,a)") "IFILE_NAME:", trim(ifile_name)
    write(*, "(a,1x,a)") "OFILE_NAME:", trim(ofile_name)
    write(*, "(a,1x,f6.3)") "X_MIN     :", x_min
    write(*, "(a,1x,f6.3)") "X_MAX     :", x_max
    write(*, "(a,1x,f6.3)") "Y_MIN     :", y_min
    write(*, "(a,1x,f6.3)") "Y_MAX     :", y_max
    write(*, "(a,1x,f6.3)") "Z_MIN     :", z_min
    write(*, "(a,1x,f6.3)") "Z_MAX     :", z_max
    write(*, "(a,1x,f6.3)") "TOL_MIN   :", tol_min
    write(*, "(a,1x,f6.3)") "TOL_MAX   :", tol_max
    write(*, "(a,1x,i0)") "TOT_GENE  :", tot_gene
    write(*, "(a,1x)", advance = "no") "TO_ORIGIN :"
    if (is_translate_to_origin) then
        write(*, "(a)") "YES"
    else
        write(*, "(a)") "NO"
    end if
    write(*, "()")

    ! release argv
    deallocate(argv)

    ! something like "call random_seed()"
    call random_init_seed()

    call cpu_time(time_start)

    mol_in = read_xyz(ifile_name)
    ! if the input file is already located its geometry center
    ! at the origin of the coordinates, hence no need to translate it to origin.
    if (is_translate_to_origin) then
        call translate_molecule_to_origin(mol_in)
        open(write_to_unit, file = trim(ifile_name), status = "replace", action = "write", &
            position = "rewind", iostat = write_file_status)
        call write_xyz(mol_in, write_to_unit)
        close(write_to_unit)
    end if

    open(write_to_unit, file = trim(ofile_name), status = "replace", &
         action = "write", iostat = write_file_status)

    if (write_file_status /= 0) then
        write(*, "(a,a,a)") "File """, trim(ofile_name), """ cannot be created!"
        stop "File cannot be created!"
    end if

    tried = 0
    loop_rotate: do num_gene = 1, tot_gene
        mol_tmp = mol_in
        ! call rotate_molecule_random(mol_tmp)
        call rotate_molecule_random(mol_tmp)
        ! rotate once, then translate randomly until suits the threshold condition.
        loop_translate: do while(.true.)
            mol_out = mol_tmp
            call translate_molecule_random(mol_out, x_min, x_max, y_min, y_max, z_min, z_max)
            tried = tried + 1
            if (check_minimal_distance_acceptable(mol_in, mol_out, tol_min, tol_max)) exit            
        end do loop_translate
        ! write(*, "(f10.6)") get_minimal_distance(mol_in, mol_out)
        call write_xyz(mol_out, write_to_unit)
        call print_progress_bar(dble(num_gene) / dble(tot_gene))
    end do loop_rotate

    write (*, "(/,a,i8,a,i8,a)") "Tried ", tried, " times, generated ", tot_gene, " ."

    close(write_to_unit)

    call destroy_molecule(mol_in)
    call destroy_molecule(mol_tmp)
    call destroy_molecule(mol_out)

    call cpu_time(time_end)

    write(*, "(a,f8.1,a)") "Time elapsed: ", (time_end - time_start), " s."

    stop
end program main

