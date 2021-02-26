module quaternion_module
    implicit none

    real(kind=8), parameter :: pi = 3.14159265358979323846264

    type :: quaternion
        real(kind=8) :: re
        real(kind=8) :: im(3)
    end type quaternion

    interface operator(+)
        module procedure quaternion_plus
    end interface operator(+)

    interface operator(-)
        module procedure quaternion_minus
    end interface operator(-)

    interface operator(*)
        module procedure quaternion_multiply
        module procedure quaternion_multiply_real
    end interface operator(*)

    contains

    subroutine quaternion_print(a, form, file_unit)
        implicit none
        type(quaternion), intent(in) :: a
        character(kind=1,len=*), optional, intent(in) :: form
        integer(kind=4), optional, intent(in) :: file_unit
        character(kind=1,len=32) :: actual_form
        integer(kind=4) :: actual_file_unit

        if (present(form)) then
            actual_form = form
        else
            actual_form = '(4(1x,f10.6))'
        end if
        if (.not. present(file_unit)) then
            actual_file_unit = 6
        else
            actual_file_unit = file_unit
        end if

        write(actual_file_unit, trim(actual_form)) a%re, a%im

        return
    end subroutine quaternion_print

    type(quaternion) function quaternion_plus(a, b)
        implicit none
        type(quaternion), intent(in) :: a, b

        quaternion_plus%re = a%re + b%re
        quaternion_plus%im = a%im + b%im

        return
    end function quaternion_plus

    type(quaternion) function quaternion_minus(a, b)
        implicit none
        type(quaternion), intent(in) :: a, b

        quaternion_minus%re = a%re - b%re
        quaternion_minus%im = a%im - b%im

        return
    end function quaternion_minus

    type(quaternion) function quaternion_multiply_real(a, b)
        implicit none
        type(quaternion), intent(in) :: a
        real(kind=8), intent(in) :: b

        quaternion_multiply_real%re = a%re * b
        quaternion_multiply_real%im = a%im * b

        return
    end function quaternion_multiply_real

    type(quaternion) function quaternion_divide_by_real(a, b)
        implicit none
        type(quaternion), intent(in) :: a
        real(kind=8), intent(in) :: b

        quaternion_divide_by_real%re = a%re / b
        quaternion_divide_by_real%im = a%im / b

        return
    end function quaternion_divide_by_real

    type(quaternion) function quaternion_conjugate(a)
        implicit none
        type(quaternion), intent(in) :: a

        quaternion_conjugate%re = a%re
        quaternion_conjugate%im = - a%im

        return
    end function quaternion_conjugate

    real(kind=8) function quaternion_norm_square(a)
        implicit none
        type(quaternion), intent(in) :: a

        quaternion_norm_square = a%re ** 2 + sum((a%im) ** 2)

        return
    end function quaternion_norm_square

    real(kind=8) function quaternion_norm(a)
        implicit none
        type(quaternion), intent(in) :: a

        quaternion_norm = dsqrt(quaternion_norm_square(a))

        return
    end function quaternion_norm

    type(quaternion) function quaternion_reverse(a)
        implicit none
        type(quaternion), intent(in) :: a

        quaternion_reverse = quaternion_divide_by_real(quaternion_conjugate(a), quaternion_norm_square(a))

        return
    end function quaternion_reverse

    type(quaternion) function quaternion_multiply(a, b)
        use vector_operation_module
        implicit none
        type(quaternion), intent(in) :: a, b

        quaternion_multiply%re = a%re * b%re - sum(a%im * b%im)
        quaternion_multiply%im = a%re * b%im + b%re * a%im + ((a%im) .cross. (b%im))

        return
    end function quaternion_multiply

    type(quaternion) function quaternion_rotator(axis_x, axis_y, axis_z, angle)
        implicit none
        real(kind=8), intent(in) :: axis_x, axis_y, axis_z, angle
        integer(kind=4) :: i

        quaternion_rotator%im = (/axis_x, axis_y, axis_z/)
        quaternion_rotator%im = quaternion_rotator%im / dsqrt(sum((quaternion_rotator%im) ** 2))
        quaternion_rotator%re = dcos(angle / 2.0D0)
        quaternion_rotator%im = quaternion_rotator%im * dsin(angle / 2.0D0)

        return
    end function quaternion_rotator

    type(quaternion) function quaternion_random_rotator()
        ! Note: random_seed() must be called once before calling this function!
        use vector_operation_module
        implicit none
        real(kind=8) :: t

        quaternion_random_rotator%im = vector_random_normal()
        call random_number(t)
        t = t * pi
        ! t = theta / 2, theta from 0 to 2 * pi, t from 0 to pi
        quaternion_random_rotator%re = dcos(t)
        quaternion_random_rotator%im = quaternion_random_rotator%im * dsin(t)

        return
    end function quaternion_random_rotator

    subroutine rotate_vector(vec, rot)
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

end module quaternion_module

