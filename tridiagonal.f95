subroutine tridiagonal_LU(n,Ad,Au,Al,deter)
    !Calcula la factorizacion LU y determinante de una matriz tridiagonal
    use mod_clreal
    implicit none
    
    integer, intent(in) :: n
    real(clreal), intent(inout) :: Ad(n),Au(n-1),Al(n-1)
    real(clreal), intent(out)::deter
    integer :: i
    real(clreal) :: eps = 1.e-12

    deter = 1

    do i = 1, n-1
        if ( abs(Ad(i)) < eps ) then
            stop "Elemento diagonal nulo"
        end if
        Al(i)=Al(i)/Ad(i)
        Ad(i+1)=Ad(i+1)-Al(i)*Au(i)
        deter = deter * Ad(i)
    end do
    if ( abs(Ad(n)) < eps ) then
        stop "Elemento diagonal nulo"
    end if
    deter = deter * Ad(n)
end subroutine tridiagonal_LU
