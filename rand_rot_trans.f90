! to randomly rotate and translate pentacene, then to make sure its position is proper.

program main
    use molecule_module
    use progress_bar_module
    use rand_init_module
    ! use ieee_arithmetic
    implicit none
    integer(kind=4) :: argc
    character(kind=1,len=512) :: argv0
    character(kind=1,len=256), allocatable :: argv(:)
    integer(kind=4) :: arg_status
    character(kind=1,len=256) :: ifile_name, ofile_name
    type(molecule) :: mol_in, mol_out, mol_tmp
    integer(kind=4) :: write_to_unit
    integer(kind=4) :: write_file_status
    integer(kind=4) :: num_gene
    integer(kind=4) :: tried
    integer(kind=4) :: max_gene = 100
    real(kind=8) :: time_start, time_end
    ! real(kind=8), parameter :: trans_min_len = 0.0D0, trans_max_len = 8.0D0
    integer(kind=4) :: i, j

    ! get command arguments, not argc in C, this is the argc except argv0.
    argc = command_argument_count()
    call get_command_argument(0, argv0, arg_status)
    if (argc > 0) then
        allocate(argv(argc))
        do i = 1, argc
            call get_command_argument(i, argv(i), arg_status)
        end do
    end if

    ! currently only uses the first argument as max_gene.
    if (argc >= 1) then
        read(argv(1), *) max_gene
    else
        max_gene = 100
    end if

    ! something like "call random_seed()"
    call random_init_seed()

    call cpu_time(time_start)

    ! write(*, "(A)") "Input file name for reading: "
    ! read(*, "(A)") ifile_name
    ifile_name = "pentacene.xyz"

    mol_in = read_xyz(ifile_name)
    ! the input file "pentacene.xyz" is already located its geometry center
    ! at the origin of the coordinates, hence no need to translate it to origin.
    ! translate_molecule_to_origin(mol_in)

    ! write(*, "(A)") "Input file name for writing: "
    ! read(*, "(A)") ofile_name
    ofile_name = "traj.xyz"
    write_to_unit = 11
    open(write_to_unit, file = trim(ofile_name), status = "replace", &
         access = "sequential", form = "formatted", action = "write", iostat = write_file_status)

    if (write_file_status /= 0) then
        write(*, "(a)") "File """ // trim(ofile_name) // """ cannot be created!"
        stop "File cannot be created!"
    end if

    tried = 0
    loop_rotate: do num_gene = 1, max_gene
        mol_tmp = mol_in
        ! call rotate_molecule_random(mol_tmp)
        call rotate_molecule_random(mol_tmp)
        ! rotate once, then translate randomly until suits the threshold condition.
        loop_translate: do while(.true.)
            mol_out = mol_tmp
            call translate_molecule_random(mol_out, 3.0D0, 5.0D0, &
                                                    0.0D0, 18.125D0, &
                                                    0.0D0, 9.006D0)
            tried = tried + 1
            if (check_minimal_distance_acceptable(mol_in, mol_out, 3.0D0, 5.5D0)) exit            
        end do loop_translate
        ! write(*, "(f10.6)") get_minimal_distance(mol_in, mol_out)
        call write_xyz(mol_out, write_to_unit)
        call print_progress_bar(dble(num_gene) / dble(max_gene))
    end do loop_rotate

    write (*, "(/,a,i8,a,i8,a)") "Tried ", tried, " times, generated ", max_gene, " ."

    close(write_to_unit)

    call destroy_molecule(mol_in)
    call destroy_molecule(mol_out)

    call cpu_time(time_end)

    if (argc > 0) deallocate(argv)

    write(*, "(a,f8.1,a)") "Time elapsed: ", (time_end - time_start), " s."

    stop
end program main

