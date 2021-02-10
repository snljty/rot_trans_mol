module progress_bar_module
    implicit none

    contains

    subroutine print_progress_bar(progress)
        implicit none
        real(kind=8), intent(in) :: progress
        integer(kind=4), parameter :: total_chars = 50
        integer(kind=4) :: left_chars

        left_chars = nint(progress * total_chars)
        write(*, '(a,f5.1,a)', advance = 'no') '[' // repeat('-', left_chars) &
        // repeat(' ', total_chars - left_chars) // ']        ', &
        progress * 100.0D0, ' %' // char(13)

        return   
    end subroutine print_progress_bar
end module progress_bar_module

