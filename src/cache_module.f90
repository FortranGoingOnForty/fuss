module cache_module
    use types_module
    implicit none

    ! File cache type for storing git command results
    type :: file_cache
        real(8) :: timestamp
        type(file_entry), allocatable :: files(:)
        integer :: n_files
        logical :: valid
    end type

    ! Global caches for dirty files and all files
    type(file_cache) :: dirty_cache, all_cache

    ! Cache time-to-live (500ms = 0.5 seconds)
    ! After this time, cache is considered stale
    real(8), parameter :: CACHE_TTL = 0.5d0

contains

    ! Check if a cache entry is still valid
    function cache_valid(cache) result(valid)
        type(file_cache), intent(in) :: cache
        logical :: valid
        real(8) :: current_time

        if (.not. cache%valid) then
            valid = .false.
            return
        end if

        call cpu_time(current_time)
        valid = (current_time - cache%timestamp) < CACHE_TTL
    end function cache_valid

    ! Invalidate a cache (mark as stale)
    subroutine invalidate_cache(cache)
        type(file_cache), intent(inout) :: cache
        cache%valid = .false.
    end subroutine invalidate_cache

    ! Invalidate all caches (called after state-changing operations)
    subroutine invalidate_all_caches()
        call invalidate_cache(dirty_cache)
        call invalidate_cache(all_cache)
    end subroutine invalidate_all_caches

    ! Update cache with new data
    subroutine update_cache(cache, files, n_files)
        type(file_cache), intent(inout) :: cache
        type(file_entry), intent(in) :: files(:)
        integer, intent(in) :: n_files
        integer :: i

        ! Free old data
        if (allocated(cache%files)) deallocate(cache%files)

        ! Allocate and copy new data
        allocate(cache%files(n_files))
        do i = 1, n_files
            cache%files(i) = files(i)
        end do

        cache%n_files = n_files
        call cpu_time(cache%timestamp)
        cache%valid = .true.
    end subroutine update_cache

    ! Retrieve cached data
    subroutine get_cached_files(cache, files, n_files)
        type(file_cache), intent(in) :: cache
        type(file_entry), allocatable, intent(out) :: files(:)
        integer, intent(out) :: n_files
        integer :: i

        n_files = cache%n_files

        if (allocated(files)) deallocate(files)
        allocate(files(n_files))

        do i = 1, n_files
            files(i) = cache%files(i)
        end do
    end subroutine get_cached_files

    ! Initialize caches (call at program start)
    subroutine init_caches()
        dirty_cache%valid = .false.
        dirty_cache%timestamp = 0.0d0
        dirty_cache%n_files = 0

        all_cache%valid = .false.
        all_cache%timestamp = 0.0d0
        all_cache%n_files = 0
    end subroutine init_caches

end module cache_module
