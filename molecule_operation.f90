! read a xyz file then rotate a ramdom angle.

module molecule_module
    implicit none

    integer(kind=4), parameter :: max_ele = 112
    real(kind=8), parameter :: amas(max_ele) = &
        (/  1.008D0,   4.003D0,   6.941D0,   9.012D0,  10.811D0, &
           12.011D0,  14.007D0,  15.999D0,  18.998D0,  20.180D0, &
           22.990D0,  24.305D0,  26.982D0,  28.086D0,  30.974D0, &
           32.066D0,  35.453D0,  39.948D0,  39.098D0,  40.078D0, &
           44.956D0,  47.867D0,  50.942D0,  51.996D0,  54.938D0, &
           55.845D0,  58.933D0,  58.693D0,  63.546D0,  65.380D0, &
           69.723D0,  72.630D0,  74.922D0,  78.971D0,  79.904D0, &
           83.798D0,  85.468D0,  87.620D0,  88.906D0,  91.224D0, &
           92.906D0,  95.950D0,  98.906D0, 101.070D0, 102.910D0, &
          106.420D0, 107.870D0, 112.410D0, 114.820D0, 118.710D0, &
          121.760D0, 127.600D0, 126.900D0, 131.290D0, 132.910D0, &
          137.330D0, 138.910D0, 140.120D0, 140.910D0, 144.240D0, &
          144.913D0, 150.360D0, 151.960D0, 157.250D0, 158.930D0, &
          162.500D0, 164.930D0, 167.260D0, 168.930D0, 173.050D0, &
          174.970D0, 178.490D0, 180.950D0, 183.840D0, 186.210D0, &
          190.230D0, 192.220D0, 195.080D0, 196.970D0, 200.590D0, &
          204.383D0, 207.200D0, 208.980D0, 208.982D0, 209.987D0, &
          222.017D0, 223.020D0, 226.025D0, 227.027D0, 232.038D0, &
          231.036D0, 238.029D0, 237.048D0, 244.064D0, 243.061D0, &
          247.070D0, 247.070D0, 251.080D0, 252.083D0, 257.095D0, &
          258.098D0, 259.101D0, 262.110D0, 267.122D0, 270.131D0, &
          269.129D0, 270.133D0, 270.134D0, 278.156D0, 281.165D0, &
          281.166D0, 285.177D0 /)
    real(kind=8), parameter :: vrad(max_ele) = &
        (/ 1.20D0, 1.43D0, 2.12D0, 1.98D0, 1.91D0, 1.77D0, &
           1.66D0, 1.50D0, 1.46D0, 1.58D0, 2.50D0, 2.51D0, &
           2.25D0, 2.19D0, 1.90D0, 1.89D0, 1.82D0, 1.83D0, &
           2.73D0, 2.62D0, 2.58D0, 2.46D0, 2.42D0, 2.45D0, &
           2.45D0, 2.44D0, 2.40D0, 2.40D0, 2.38D0, 2.39D0, &
           2.32D0, 2.29D0, 1.88D0, 1.82D0, 1.86D0, 2.25D0, &
           3.21D0, 2.84D0, 2.75D0, 2.52D0, 2.56D0, 2.45D0, &
           2.44D0, 2.46D0, 2.44D0, 2.15D0, 2.53D0, 2.49D0, &
           2.43D0, 2.42D0, 2.47D0, 1.99D0, 2.04D0, 2.06D0, &
           3.48D0, 3.03D0, 2.98D0, 2.88D0, 2.92D0, 2.95D0, &
           0.00D0, 2.90D0, 2.87D0, 2.83D0, 2.79D0, 2.87D0, &
           2.81D0, 2.83D0, 2.79D0, 2.80D0, 2.74D0, 2.63D0, &
           2.53D0, 2.57D0, 2.49D0, 2.48D0, 2.41D0, 2.29D0, &
           2.32D0, 2.45D0, 2.47D0, 2.60D0, 2.54D0, 0.00D0, &
           0.00D0, 0.00D0, 0.00D0, 0.00D0, 2.80D0, 2.93D0, &
           2.88D0, 2.71D0, 2.82D0, 2.81D0, 2.83D0, 3.05D0, &
           3.40D0, 3.05D0, 2.70D0, 0.00D0, 0.00D0, 0.00D0, &
           0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, 0.00D0, &
           0.00D0, 0.00D0, 0.00D0, 0.00D0 /)
    real(kind=8), parameter :: crad(max_ele) = &
        (/ 0.31D0, 0.28D0, 1.28D0, 0.96D0, 0.84D0, 0.76D0, &
           0.71D0, 0.66D0, 0.57D0, 0.58D0, 1.66D0, 1.41D0, &
           1.21D0, 1.11D0, 1.07D0, 1.05D0, 1.02D0, 1.06D0, &
           2.03D0, 1.76D0, 1.70D0, 1.60D0, 1.53D0, 1.39D0, &
           1.39D0, 1.32D0, 1.26D0, 1.24D0, 1.32D0, 1.22D0, &
           1.22D0, 1.20D0, 1.19D0, 1.20D0, 1.20D0, 1.16D0, &
           2.20D0, 1.95D0, 1.90D0, 1.75D0, 1.64D0, 1.54D0, &
           1.47D0, 1.46D0, 1.42D0, 1.39D0, 1.45D0, 1.44D0, &
           1.42D0, 1.39D0, 1.39D0, 1.38D0, 1.39D0, 1.40D0, &
           2.44D0, 2.15D0, 2.07D0, 2.04D0, 2.03D0, 2.01D0, &
           1.99D0, 1.98D0, 1.98D0, 1.96D0, 1.94D0, 1.92D0, &
           1.92D0, 1.89D0, 1.90D0, 1.87D0, 1.87D0, 1.75D0, &
           1.70D0, 1.62D0, 1.51D0, 1.44D0, 1.41D0, 1.36D0, &
           1.36D0, 1.32D0, 1.45D0, 1.46D0, 1.48D0, 1.40D0, &
           1.50D0, 1.50D0, 2.60D0, 2.21D0, 2.15D0, 2.06D0, &
           2.00D0, 1.96D0, 1.90D0, 1.87D0, 1.80D0, 1.69D0, &
           1.68D0, 1.68D0, 1.65D0, 1.67D0, 1.73D0, 1.76D0, &
           1.61D0, 1.57D0, 1.49D0, 1.43D0, 1.41D0, 1.34D0, &
           1.29D0, 1.28D0, 1.21D0, 1.22D0 /)
    character(kind=1,len=2), parameter :: asym(max_ele) = &
        (/ "H ", "He", "Li", "Be", "B ", "C ", "N ", &
           "O ", "F ", "Ne", "Na", "Mg", "Al", "Si", &
           "P ", "S ", "Cl", "Ar", "K ", "Ca", "Sc", &
           "Ti", "V ", "Cr", "Mn", "Fe", "Co", "Ni", &
           "Cu", "Zn", "Ga", "Ge", "As", "Se", "Br", &
           "Kr", "Rb", "Sr", "Y ", "Zr", "Nb", "Mo", &
           "Tc", "Ru", "Rh", "Pd", "Ag", "Cd", "In", &
           "Sn", "Sb", "Te", "I ", "Xe", "Cs", "Ba", &
           "La", "Ce", "Pr", "Nd", "Pm", "Sm", "Eu", &
           "Gd", "Tb", "Dy", "Ho", "Er", "Tm", "Yb", &
           "Lu", "Hf", "Ta", "W ", "Re", "Os", "Ir", &
           "Pt", "Au", "Hg", "Tl", "Pb", "Bi", "Po", &
           "At", "Rn", "Fr", "Ra", "Ac", "Th", "Pa", &
           "U ", "Np", "Pu", "Am", "Cm", "Bk", "Cf", &
           "Es", "Fm", "Md", "No", "Lr", "Rf", "Db", &
           "Sg", "Bh", "Hs", "Mt", "Ds", "Rg", "Cn" /)

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

        open(10, file = trim(filename), position = "rewind", status = "old", &
             access = "sequential", form = "formatted", action = "read", iostat = read_file_status)
        if (read_file_status /= 0) then
            write(*, "(a)") "File """ // trim(filename) // """ not found!"
            stop "File not found!"
        end if

        ! num_atoms
        read(10, "(a)") buf
        read(buf, *) read_xyz%num_atoms
        ! title
        read(10, "(a)") read_xyz%title
        ! atom_names and atom_coords
        allocate(read_xyz%atom_names(read_xyz%num_atoms))
        allocate(read_xyz%atom_coords(num_coords, read_xyz%num_atoms))
        do i = 1, read_xyz%num_atoms
            read(10, "(a)") buf
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

        write(tmp_str, "(i10)") mol%num_atoms
        write(file_unit, "(a)") trim(adjustl(tmp_str))
        write(file_unit, "(a)") trim(mol%title)
        do i = 1,mol%num_atoms
            write(file_unit, "(1x,a3,12x,3(2x,f12.8))") mol%atom_names(i), mol%atom_coords(:, i)
        end do

        return
    end subroutine write_xyz

    subroutine destroy_molecule(mol)
        implicit none
        type(molecule), intent(inout) :: mol

        mol%num_atoms = 0
        mol%title = ""
        if (allocated(mol%atom_names)) deallocate(mol%atom_names)
        if (allocated(mol%atom_coords)) deallocate(mol%atom_coords)
        return
    end subroutine destroy_molecule

    subroutine duplicate_molecule(mol_new, mol_old)
        implicit none
        type(molecule), intent(inout) :: mol_new
        type(molecule), intent(in) :: mol_old
        integer(kind=4), parameter :: num_coords = 3

        if (.not. allocated(mol_new%atom_names) .or. &
           (mol_new%num_atoms /= mol_old%num_atoms)) then
            call destroy_molecule(mol_new)
            mol_new%num_atoms = mol_old%num_atoms
            allocate(mol_new%atom_names(mol_new%num_atoms))
            allocate(mol_new%atom_coords(num_coords, mol_new%num_atoms))
            mol_new%title = trim(mol_old%title)
            mol_new%atom_names = mol_old%atom_names
        else
            ! here does nothing, if you want to completely duplicate the molecule, 
            ! use "duplicate_molecule_full_info" instead. 
            ! mol_new%title = trim(mol_old%title)
            ! mol_new%atom_names = mol_old%atom_names
            continue
        end if
        mol_new%atom_coords = mol_old%atom_coords

        return
    end subroutine duplicate_molecule

    subroutine duplicate_molecule_full_info(mol_new, mol_old)
        implicit none
        type(molecule), intent(inout) :: mol_new
        type(molecule), intent(in) :: mol_old
        integer(kind=4), parameter :: num_coords = 3

        call destroy_molecule(mol_new)
        mol_new%num_atoms = mol_old%num_atoms
        allocate(mol_new%atom_names(mol_new%num_atoms))
        allocate(mol_new%atom_coords(num_coords, mol_new%num_atoms))
        mol_new%title = trim(mol_old%title)
        mol_new%atom_names = mol_old%atom_names
        mol_new%atom_coords = mol_old%atom_coords

        return
    end subroutine duplicate_molecule_full_info

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
        use vector_operation_module
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8), dimension(3, 3) :: rot_matrix
        integer(kind=4) :: i

        rot_matrix = vector_rotate_random_matrix()
        do i = 1, mol%num_atoms
            mol%atom_coords(:, i) = matmul(rot_matrix, mol%atom_coords(:, i))
        end do

        return
    end subroutine rotate_molecule_random

    subroutine rotate_molecule_xyz(mol, rot_x, rot_y, rot_z)
        ! Note: the sequence is important! 
        ! Rotate in different sequences will result in different results,
        ! even if the angles corresponding to each axes are not changed.
        use quaternion_module
        use vector_operation_module
        implicit none
        type(molecule), intent(inout) :: mol
        real(kind=8), intent(in) :: rot_x, rot_y, rot_z
        integer(kind=4) :: i

        do i = 1, mol%num_atoms
            mol%atom_coords(:, i) = matmul(vector_rotate_xyz_matrix(rot_x, rot_y, rot_z), &
                mol%atom_coords(:, i))
        end do

        return
    end subroutine rotate_molecule_xyz

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

        trans = sum(mol%atom_coords, dim = 2) / mol%num_atoms
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
                if (tmp_distance < get_minimal_distance) then
                    get_minimal_distance = tmp_distance
                end if
            end do loop_j
        end do loop_i

        return
    end function get_minimal_distance

    ! get the mininal distance between atoms in each molecule.
    ! using the function from "rand_rot_trans.f90" may be time-consuming, hence 
    ! a new impliment is given here.
    function check_minimal_distance_acceptable(mol1, mol2, lower_thres, higher_thres)
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
                if (curr_dist <= min_dist) then
                    min_dist = curr_dist
                    if (min_dist < lower_thres) return
                end if
            end do
        end do
        if ((min_dist < lower_thres) .or. (min_dist > higher_thres)) return
        check_minimal_distance_acceptable = .true.
        return
    end function check_minimal_distance_acceptable

end module molecule_module

