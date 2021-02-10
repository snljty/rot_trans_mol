! read a xyz file then rotate a ramdom angle.

module molecule_module
    implicit none

    type :: molecule
        integer(kind=4) :: num_atoms
        character(kind=1,len=256) :: title
        character(kind=1,len=3), allocatable :: atom_names(:)
        real(kind=8), allocatable :: atom_coords(:,:)
    end type molecule

    interface assignment(=)
        module procedure duplicate_molecule
    end interface

    contains

    function read_xyz(filename)
        implicit none
        character(kind=1,len=256), intent(in) :: filename
        integer(kind=4) :: read_file_status
        character(kind=1,len=256) :: buf
        type(molecule) :: read_xyz
        integer(kind=4) :: i
        integer(kind=4), parameter :: num_coords = 3

        open(10, file = trim(filename), position = 'rewind', status = 'old', &
             access = 'sequential', form = 'formatted', action = 'read', iostat = read_file_status)
        if (read_file_status .ne. 0) then
            write(*, '(a)') 'File ''' // trim(filename) // ''' not found!'
            stop 'File not found!'
        end if

        ! num_atoms
        read(10, '(a)') buf
        read(buf, *) read_xyz%num_atoms
        ! title
        read(10, '(a)') read_xyz%title
        ! atom_names and atom_coords
        allocate(read_xyz%atom_names(read_xyz%num_atoms))
        allocate(read_xyz%atom_coords(num_coords, read_xyz%num_atoms))
        do i = 1, read_xyz%num_atoms
            read(10, '(a)') buf
            read(buf, *) read_xyz%atom_names(i), read_xyz%atom_coords(:, i)
        end do
        close(10)

        return
    end function read_xyz

    subroutine write_xyz(mol, file_unit)
        ! note: the file needs to be opened before!
        implicit none
        type(molecule), intent(in) :: mol
        integer(kind=4), intent(in) :: file_unit
        integer(kind=4) :: i
        character(kind=1,len=10) :: tmp_str

        write(tmp_str, '(i10)') mol%num_atoms
        write(file_unit, '(a)') trim(adjustl(tmp_str))
        write(file_unit, '(a)') trim(mol%title)
        do i = 1,mol%num_atoms
            write(file_unit, '(1x,a3,12x,3(2x,f12.8))') mol%atom_names(i), mol%atom_coords(:, i)
        end do

        return
    end subroutine write_xyz

    subroutine destroy_molecule(mol)
        implicit none
        type(molecule), intent(inout) :: mol

        mol%num_atoms = 0
        mol%title = ''
        if (allocated(mol%atom_names)) then
            deallocate(mol%atom_names)
        end if
        if (allocated(mol%atom_coords)) then
            deallocate(mol%atom_coords)
        end if
        return
    end subroutine destroy_molecule

    subroutine duplicate_molecule(mol_new, mol_old)
        implicit none
        type(molecule), intent(inout) :: mol_new
        type(molecule), intent(in) :: mol_old
        integer(kind=4), parameter :: num_coords = 3

        if (.not. allocated(mol_new%atom_names) .or. &
           (mol_new%num_atoms .ne. mol_old%num_atoms)) then
            call destroy_molecule(mol_new)
            mol_new%num_atoms = mol_old%num_atoms
            allocate(mol_new%atom_names(mol_new%num_atoms))
            allocate(mol_new%atom_coords(mol_new%num_atoms, num_coords))
        end if
        ! mol_new%title = trim(mol_old%title)
        ! mol_new%atom_names = mol_old%atom_names
        mol_new%atom_coords = mol_old%atom_coords

        return
    end subroutine duplicate_molecule

    subroutine duplicate_molecule_full_info(mol_new, mol_old)
        implicit none
        type(molecule), intent(inout) :: mol_new
        type(molecule), intent(in) :: mol_old
        integer(kind=4), parameter :: num_coords = 3

        if (.not. allocated(mol_new%atom_names) .or. &
           (mol_new%num_atoms .ne. mol_old%num_atoms)) then
            call destroy_molecule(mol_new)
            mol_new%num_atoms = mol_old%num_atoms
            allocate(mol_new%atom_names(mol_new%num_atoms))
            allocate(mol_new%atom_coords(mol_new%num_atoms, num_coords))
        end if
        mol_new%title = trim(mol_old%title)
        mol_new%atom_names = mol_old%atom_names
        mol_new%atom_coords = mol_old%atom_coords

        return
    end subroutine duplicate_molecule_full_info

    subroutine rotate_vector(vec, rot)
        use quaternion_module
        implicit none
        real(kind=8), intent(inout) :: vec(3)
        type(quaternion), intent(in) :: rot
        type(quaternion) :: t

        t%re = 0.0D0
        t%im = vec
        t = rot * t * quaternion_conjugate(rot)
        vec = t%im

        return
    end subroutine rotate_vector

    subroutine rotate_molecule(mol, axis_x, axis_y, axis_z, angle)
        use quaternion_module
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8), intent(in) :: axis_x, axis_y, axis_z, angle
        real(kind=8) :: axis_old_len
        type(quaternion) :: r
        integer(kind=4) :: i

        r%im = (/axis_x, axis_y, axis_z/)
        axis_old_len = dsqrt(sum((r%im) ** 2))
        r%im = r%im / axis_old_len
        r%re = dcos(angle / 2.0D0)
        r%im = r%im * dsin(angle / 2.0D0)
        do i = 1, mol%num_atoms
            call rotate_vector(mol%atom_coords(:, i), r)
        end do

        return
    end subroutine rotate_molecule

    subroutine rotate_molecule_random(mol)
        ! Note: random_seed() must be called once before calling this function!
        use quaternion_module
        implicit none
        type(molecule), intent(inout) :: mol
        integer(kind=4) :: i
        type(quaternion) :: r

        r = quaternion_random_rotator()
        do i = 1, mol%num_atoms
            call rotate_vector(mol%atom_coords(:, i), r)
        end do

        return
    end subroutine rotate_molecule_random

    subroutine translate_molecule(mol, trans_x, trans_y, trans_z)
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8), intent(in) :: trans_x, trans_y, trans_z
        real(kind=8) :: trans(3)
        integer(kind=4) :: i

        trans = (/trans_x, trans_y, trans_z/)
        do i = 1, mol%num_atoms
            mol%atom_coords(:, i) = mol%atom_coords(:, i) + trans
        end do

        return
    end subroutine translate_molecule

    subroutine translate_molecule_to_origin(mol)
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8) :: trans(3)
        integer(kind=4) :: i

        trans = sum(mol%atom_coords, 2) / mol%num_atoms
        do i = 1, mol%num_atoms
            mol%atom_coords(:, i) = mol%atom_coords(:, i) - trans
        end do

        return
    end subroutine translate_molecule_to_origin

    subroutine translate_molecule_random(mol, x_min, x_max, y_min, y_max, z_min, z_max)
        ! Note: random_seed() must be called once before calling this function!
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8), intent(in) :: x_min, x_max, y_min, y_max, z_min, z_max
        integer(kind=4), parameter :: num_coords = 3
        real(kind=8) :: trans(num_coords)
        integer(kind=4) :: i
        integer(kind=4) :: j
        real(kind=8) :: trans_min(num_coords), trans_max(num_coords)

        trans_min = (/x_min, y_min, z_min/)
        trans_max = (/x_max, y_max, z_max/)

        do j = 1, num_coords
            call random_number(trans(j))
            trans(j) = trans(j) * abs(trans_max(j) - trans_min(j)) + min(trans_min(j), trans_max(j))
        end do

        do i = 1, mol%num_atoms
            mol%atom_coords(:, i) = mol%atom_coords(:, i) + trans
        end do

        return
    end subroutine translate_molecule_random

    function vector_distance(vec1, vec2)
        implicit none
        real(kind=8), dimension(3), intent(in) :: vec1
        real(kind=8), dimension(3), intent(in) :: vec2
        real(kind=8) :: vector_distance

        vector_distance = dsqrt(sum((vec1 - vec2) ** 2))

        return
    end function vector_distance

    function get_minimal_distance(mol1, mol2)
        implicit none
        type(molecule), intent(in) :: mol1, mol2
        real(kind=8) :: get_minimal_distance
        real(kind=8) :: tmp_distance
        integer(kind=4) :: i, j

        get_minimal_distance = vector_distance(mol1%atom_coords(:, 1), mol2%atom_coords(:, 1))

        loop_i: do i = 1, mol1%num_atoms
            loop_j: do j = 1, mol2%num_atoms
                tmp_distance = vector_distance(mol1%atom_coords(:, i), mol2%atom_coords(:, j))
                if (tmp_distance .lt. get_minimal_distance) then
                    get_minimal_distance = tmp_distance
                end if
            end do loop_j
        end do loop_i

        return
    end function get_minimal_distance

end module molecule_module

