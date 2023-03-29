!De la igualdad A*A⁻¹=I, podemos considerar las columnas de A⁻¹ como (u₁|...|uₙ)
!Entonces podemos resolver n sistemas lineales A*uᵢ=Iᵢ siendo Iᵢ un vector todo 0s excepto 1 en la posición i

subroutine inversa_superior(n,A)
    use mod_clreal
    implicit none

    integer, intent(in)::n
    integer::i
    real(clreal), intent(inout)::A(n,n)
    real(clreal)::Inv(n,n),b(n)

    do i = 1, n
        b=0
        b(i)=1
        CALL remonte(n,A,b,Inv(:,i))
    end do
    A=Inv
end subroutine inversa_superior

subroutine inversa(n,A)
    use mod_clreal
    implicit none

    real(clreal), intent(inout) :: A(n,n)
    integer,intent(in)::n
    integer::i
    real(clreal) :: Acopia(n,n), b(n), Inv(n,n), null

    Acopia=A

    do i=1,n
        a=Acopia
        b=0
        b(i)=1
        CALL gauss(n,a,b,null)
        CALL remonte(n,a,b,Inv(:,i))
    end do
end subroutine inversa
