PROGRAM main
    
    USE mod_clreal
    IMPLICIT NONE

    INTEGER::n
    REAL(clreal),ALLOCATABLE::A(:,:),b(:),u(:),r(:),Acopia(:,:),Bcopia(:)
    REAL(clreal)::deter

    read*,n
    print*,"Tamaño: ",n

    ALLOCATE(A(n,n),b(n),u(n),r(n),Acopia(n,n),Bcopia(n))

    CALL lecmat(A,n)
    print*, "Matriz A:"
    CALL prinmat(A,n)

    Acopia=A

    read*,b
    print*, "Vector de términos independientes:", b

    Bcopia=b

    CALL gauss(n,A,b,deter)
    print*, "Determinante: ",deter

    print*, "Nueva matriz reducida:"
    CALL prinmat(A,n)
    print*, "Nuevo vector independiente:", b

    CALL remonte(n,A,b,u)

    print*, "Solución: ",u


    ! CALL inversa_superior(n,A)

    ! print*, "Inversa:"
    ! CALL prinmat(A,n)

    A=Acopia
    B=Bcopia

    CALL gauss_LU(n,A)

    CALL descenso_L(n,A,b,u)

    CALL remonte(n,A,u,b)

    print*, "Solucion:", b

    A=Acopia

    CALL inversa(n,A)

    print*, "Matriz inversa: "
    CALL prinmat(A,n)

END PROGRAM main
