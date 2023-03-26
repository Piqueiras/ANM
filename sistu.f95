subroutine remonte(n,A,b,u)
    !Subrutina que soluciona el sistema de ecuaciones lineales A*u = b cuando A es una matriz triangular SUPERIOR
    !El sistema se soluciona por remonte
    !Por ejemplo, con 3 variables, como el sistema es triangular superior se tiene que 
    ! a(1,1)x + a(1,2)y + a(1,3)z = b(1)
    !           a(2,2)y + a(2,3)z = b(2)
    !                     a(3,3)z = b(3)
    !Siendo u = (x,y,z)
    !Entonces z = u(3) = b(3)/a(3,3) 
    !(cuidao con dividir por 0)
    !Luego para calcular y, podemos tambien pasar al otro lado, porque ya conocemos z (es u(3))
    !y = u(2) = (b(2) - a(2,3)*z)/a(2,2)

    !Pues eso
    use mod_clreal
    implicit none

    integer, intent(in)::n
    real(clreal), intent(in)::A(n,n)
    real(clreal), intent(inout)::b(n)
    real(clreal), intent(out)::u(n)
    integer::i
    
    do i=n,1,-1
        u(i)=b(i)/a(i,i)
        b(1:i-1)=b(1:i-1)-A(1:i-1,i)*u(i)
    end do
end subroutine remonte