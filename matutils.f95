subroutine lecmat(a,n)
    !Subrutina para leer matriz, porque Fortran tiene daño cerebral e indexa en 1
    use mod_clreal
    implicit none
    integer::i
    integer,INTENT(IN)::n
    real(clreal),INTENT(OUT)::a(n,n)
    do i = 1, n
        read*, a(i,:)
    end do
end subroutine lecmat

subroutine prinmat(a,n)
    !Subrutina para escribir matriz, porque Fortran tiene daño cerebral y guarda por columnas
    use mod_clreal
    implicit none
    integer::i
    integer,INTENT(IN)::n
    real(clreal),INTENT(IN)::a(n,n)
    do i = 1, n
        print*, a(i,:)
    end do
end subroutine prinmat