! initialize random seed
module rand_init_module
    implicit none

    contains

    subroutine random_init()
        implicit none
        integer(kind=4) :: rand_seed_size
        integer(kind=4), allocatable :: rand_seed(:)
        integer(kind=4) :: time_c, time_cr, time_cm

        call system_clock(time_c, time_cr, time_cm)
        call random_seed(size = rand_seed_size)
        allocate(rand_seed(rand_seed_size))
        call random_seed(get = rand_seed)
        rand_seed = rand_seed + time_c
        call random_seed(put = rand_seed)
        deallocate(rand_seed)

        return
    end subroutine random_init
end module rand_init_module

