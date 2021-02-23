! to randomly rotate and translate pentacene, then to make sure its position is proper.

! get the mininal distance between atoms in each molecule.
! using the function from 'rand_rot_trans.f90' may be time-consuming, hence 
! a new impliment is given here.
function check_minimal_distance_acceptable(mol1, mol2, lower_thres, higher_thres)
    use molecule_module
    implicit none
    type(molecule), intent(in) :: mol1, mol2
    real(kind=8), intent(in) :: lower_thres, higher_thres
    logical(kind=1) :: check_minimal_distance_acceptable
    integer(kind=4) :: i, j
    real(kind=8) :: curr_dist, min_dist

    check_minimal_distance_acceptable = .false.
    min_dist = dsqrt(sum((mol1%atom_coords(:, 1) - mol2%atom_coords(:, 1)) ** 2))
    do i = 1, mol1%num_atoms
        do j = 1, mol2%num_atoms
            curr_dist = dsqrt(sum((mol1%atom_coords(:, i) - mol2%atom_coords(:, j)) ** 2))
            if (curr_dist .le. min_dist) then
                min_dist = curr_dist
                if (min_dist .lt. lower_thres) return
            end if
        end do
    end do
    if ((min_dist .lt. lower_thres) .or. (min_dist .gt. higher_thres)) return
    check_minimal_distance_acceptable = .true.
    return
end function check_minimal_distance_acceptable

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
    logical(kind=1), external :: check_minimal_distance_acceptable

    ! get command arguments, not that argv needs to be freed at the end of the program.
    argc = command_argument_count()
    call get_command_argument(0, argv0, arg_status)
    if (argc .gt. 0) then
        allocate(argv(argc))
        do i = 1, argc
            call get_command_argument(i, argv(i), arg_status)
        end do
    end if

    ! currently only uses the first argument as max_gene.
    if (argc .ge. 1) then
        read(argv(1), *) max_gene
    else
        max_gene = 100
    end if

    ! something likes "call random_seed()"
    call random_init_seed()

    call cpu_time(time_start)

    ! write(*, '(A)') 'Input file name for reading: '
    ! read(*, '(A)') ifile_name
    ifile_name = 'pentacene.xyz'

    mol_in = read_xyz(ifile_name)
    ! the input file 'pentacene.xyz' is already located its geometry center
    ! at the origin of the coordinates, hence no need to translate it to origin.
    ! translate_molecule_to_origin(mol_in)

    ! write(*, '(A)') 'Input file name for writing: '
    ! read(*, '(A)') ofile_name
    ofile_name = 'traj.xyz'
    write_to_unit = 11
    open(write_to_unit, file = trim(ofile_name), status = 'replace', &
         access = 'sequential', form = 'formatted', action = 'write', iostat = write_file_status)

    if (write_file_status .ne. 0) then
        write(*, '(a)') 'File ''' // trim(ofile_name) // ''' cannot be created!'
        stop 'File cannot be created!'
    end if

    tried = 0
    loop_rotate: do num_gene = 1, max_gene
        mol_tmp = mol_in
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
        ! write(*, '(f10.6)') get_minimal_distance(mol_in, mol_out)
        call write_xyz(mol_out, write_to_unit)
        call print_progress_bar(dble(num_gene) / dble(max_gene))
    end do loop_rotate

    write (*, '(/,a,i8,a,i8,a)') 'Tried ', tried, ' times, generated ', max_gene, ' .'

    close(write_to_unit)

    call destroy_molecule(mol_in)
    call destroy_molecule(mol_out)

    call cpu_time(time_end)

    if (argc .gt. 0) deallocate(argv)

    write(*, '(a,f8.1,a)') 'Time elapsed: ', (time_end - time_start), ' s.'

    stop
end program main

