subroutine cholesky(n,A)
    !Subrutina que calcula la factorización A=BBt para matrices simétricas
    use mod_clreal
    implicit none
    integer,intent(in)::n
    real(clreal),intent(inout)::A(n,n)
    integer :: i,j,k

    do j=1,n
        if ( a(j,j) <= 0 ) then
            stop "Matriz no definida positiva"
        end if
        !Esquina
        do k = 1, j-1
            a(j,j)=a(j,j)-a(j,k)*a(j,k)
        end do
        a(j,j)=SQRT(a(j,j))
        !Resto columna
        do i=j+1,n
            do k = 1, j-1
                a(i,j)=a(i,j)-a(i,k)*a(j,k)
            end do
            a(i,j)=a(i,j)/a(j,j)
            a(j,i)=0
        enddo
    enddo

end subroutine cholesky
