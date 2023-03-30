program main
    use mod_clreal
    implicit none

    integer::n
    real(clreal),allocatable::AD(:),AU(:),AL(:),b(:),u(:)
    real(clreal)::deter

    read*,n
    print*,"Tamaño: ",n

    ALLOCATE(AD(n),AU(n-1),AL(n-1),b(n),u(n))

    print*, "Superdiagonal: "
    read*, AU
    print*, "Diagonal: "
    read*, AD
    print*, "Subdiagonal: "
    read*, AL

    print*, "Matriz: "
    CALL printrig(n,AU,AD,AL)

    print*, "Vector de terminos independientes"
    read*, b

    CALL tridiagonal_LU(n,AD,AU,AL,deter)

    print*, "Matriz LU: "
    CALL printrig(n,AU,AD,AL)

    print*, "Determinante: ",deter

    CALL descenso_tridiagonal(n,AL,b,u)

    CALL remonte_tridiagonal(n,AD,AU,u,b)

    print*, "Solucion:", b

end program main
