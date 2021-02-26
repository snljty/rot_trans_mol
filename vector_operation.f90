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

        do while(.true.)
            call random_number(t1)
            call random_number(t2)
            t1 = 2 * t1 - 1.0D0
            t2 = 2 * t2 - 1.0D0
            t0 = t1 ** 2 + t2 ** 2
            if (t0 .lt. 1.0D0) exit
        end do
        ret(1) = 2.0D0 * t1 * dsqrt(1.0D0 - t0)
        ret(2) = 2.0D0 * t2 * dsqrt(1.0D0 - t0)
        ret(3) = 1.0D0 - 2.0D0 * t0

        return
    end function vector_random_normal

    function vector_rotate_by_x(v, theta) result(ret)
        implicit none
        real(kind=8), intent(in), dimension(3) :: v
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3) :: ret
        real(kind=8), dimension(3, 3) :: m

        m = reshape((/1.0D0,      0.0D0,        0.0D0, &
                      0.0D0, cos(theta), - sin(theta), &
                      0.0D0, sin(theta),   cos(theta)/), (/3, 3/))
        ret = reshape(matmul(m, reshape(v, (/3, 1/))), (/3/))

        return
    end function vector_rotate_by_x

    function vector_rotate_by_y(v, theta) result(ret)
        implicit none
        real(kind=8), intent(in), dimension(3) :: v
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3) :: ret
        real(kind=8), dimension(3, 3) :: m

        m = reshape((/  cos(theta), 0.0D0, sin(theta), &
                             0.0D0, 1.0D0,      0.0D0, &
                      - sin(theta), 0.0D0, cos(theta)/), (/3, 3/))
        ret = reshape(matmul(m, reshape(v, (/3, 1/))), (/3/))

        return
    end function vector_rotate_by_y

    function vector_rotate_by_z(v, theta) result(ret)
        implicit none
        real(kind=8), intent(in), dimension(3) :: v
        real(kind=8), intent(in) :: theta
        real(kind=8), dimension(3) :: ret
        real(kind=8), dimension(3, 3) :: m

        m = reshape((/cos(theta), - sin(theta), 0.0D0, &
                      sin(theta),   cos(theta), 0.0D0, &
                           0.0D0,        0.0D0, 1.0D0/), (/3, 3/))
        ret = reshape(matmul(m, reshape(v, (/3, 1/))), (/3/))

        return
    end function vector_rotate_by_z
end module vector_operation_module

