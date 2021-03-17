module vector_operation_module
    implicit none

    interface operator(.cross.)
        module procedure vector_cross_multiply
    end interface operator(.cross.)

    contains

    function vector_cross_multiply(a, b) result(ret)
        implicit none
        real(kind=8), dimension(3), intent(in) :: a, b
        real(kind=8), dimension(3) :: ret

        ret(1) = a(2) * b(3) - a(3) * b(2)
        ret(2) = a(3) * b(1) - a(1) * b(3)
        ret(3) = a(1) * b(2) - a(2) * b(1)

        return
    end function vector_cross_multiply

    function vector_random_normal() result(ret)
        ! Note: random_seed() must be called once before calling this function!
        implicit none
        real(kind=8), dimension(3) :: ret
        real(kind=8) :: t1, t2
        real(kind=8) :: t0

        do while (.true.)
            call random_number(t1)
            call random_number(t2)
            t1 = 2 * t1 - 1.0D0
            t2 = 2 * t2 - 1.0D0
            t0 = t1 ** 2 + t2 ** 2
            if (t0 < 1.0D0) exit
        end do
        ret(1) = 2.0D0 * t1 * dsqrt(1.0D0 - t0)
        ret(2) = 2.0D0 * t2 * dsqrt(1.0D0 - t0)
        ret(3) = 1.0D0 - 2.0D0 * t0

        return
    end function vector_random_normal

    function vector_rotate_by_x_matrix(theta) result(m)
        implicit none
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3, 3) :: m
        real(kind=8) :: c
        real(kind=8) :: s

        c = cos(theta)
        s = sin(theta)

        m = reshape((/1.0D0, 0.0D0, 0.0D0, &
                      0.0D0,     c,     s, &
                      0.0D0,   - s,     c/), (/3, 3/))

        return
    end function vector_rotate_by_x_matrix

    function vector_rotate_by_y_matrix(theta) result(m)
        implicit none
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3, 3) :: m
        real(kind=8) :: c
        real(kind=8) :: s

        c = cos(theta)
        s = sin(theta)
        m = reshape((/    c, 0.0D0,   - s, &
                      0.0D0, 1.0D0, 0.0D0, &
                          s, 0.0D0,     c/), (/3, 3/))

        return
    end function vector_rotate_by_y_matrix

    function vector_rotate_by_z_matrix(theta) result(m)
        implicit none
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3, 3) :: m
        real(kind=8) :: c
        real(kind=8) :: s

        c = cos(theta)
        s = sin(theta)
        m = reshape((/    c,     s, 0.0D0, &
                        - s,     c, 0.0D0, &
                      0.0D0, 0.0D0, 1.0D0/), (/3, 3/))

        return
    end function vector_rotate_by_z_matrix

    function vector_rotate_xyz_matrix(roll, pitch, yaw) result(m)
        ! rotate around x, then around y, finally around z.
        ! it equals R_z @ R_y @ R_x @ v
        implicit none
        real(kind=8), intent(in) :: roll
        real(kind=8), intent(in) :: pitch
        real(kind=8), intent(in) :: yaw
        real(kind=8), dimension(3, 3) :: m
        real(kind=8) :: c_r
        real(kind=8) :: s_r
        real(kind=8) :: c_p
        real(kind=8) :: s_p
        real(kind=8) :: c_y
        real(kind=8) :: s_y

        m = reshape((/c_y * c_p                  , s_y * c_p                  ,     - s_p,   &
                      c_y * s_p * s_r - s_y * c_r, s_y * s_p * s_r + c_y * c_r, c_p * s_r,   &
                      c_y * s_p * c_r + s_y * s_r, s_y * s_p * c_r - c_y * s_r, c_p * c_r/), &
                    (/3, 3/))

        return
    end function vector_rotate_xyz_matrix

    function vector_rotate_random_matrix() result(m)
        ! Note: random_seed() must be called once before calling this function!
        use constant_module
        implicit none
        real(kind=8), dimension(3, 3) :: m
        real(kind=8) :: angle

        do while (.true.)
            m(:, 1) = vector_random_normal()
            if ((m(1, 1) * m(2, 1) == 0.0D0) .and. &
                (m(2, 1) * m(3, 1) == 0.0D0) .and. &
                (m(3, 1) * m(1, 1) == 0.0D0)) cycle
            exit
        end do
        m(1, 2) = 2.0D0 * m(2, 1) * m(3, 1)
        m(2, 2) =       - m(3, 1) * m(1, 1)
        m(3, 2) =       - m(1, 1) * m(2, 1)
        m(:, 2) = m(:, 2) / dsqrt(sum(m(:, 2) ** 2))
        m(:, 3) = m(:, 1) .cross. m(:, 2)
        call random_number(angle)
        angle = 2 * pi * angle
        m(:, 2) = m(:, 2) * cos(angle) + m(:, 3) * sin(angle)
        m(:, 3) = m(:, 1) .cross. m(:, 2)       

        return
    end function vector_rotate_random_matrix

end module vector_operation_module

