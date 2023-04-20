subroutine cholesky(n,A)
    !Subrutina que calcula la factorización A=BBt para matrices simétricas
    use mod_clreal
    implicit none
    integer,intent(in)::n
    real(clreal),intent(inout)::A(n,n)
    integer :: i,j,k

    do j=1,n
        !Esquina
        a(j,j)=a(j,j)-sum(a(j,1:j-1)*a(j,1:j-1))
        if ( a(j,j) <= 0 ) then
            stop "Matriz no definida positiva (o no es simétrica)"
        end if
        a(j,j)=SQRT(a(j,j))
        !Resto columna
        do i=j+1,n
            a(i,j)=a(i,j)-sum(a(i,1:j-1)*a(j,1:j-1))
            a(i,j)=a(i,j)/a(j,j)
            a(j,i)=0
        enddo
    enddo

end subroutine cholesky
