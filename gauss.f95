subroutine gauss(n,A,b,deter)
    !Subrutina que emplea reducción de Gauss para convertir la matriz en triangular superior
    !En el resto de elementos se guardan los factores de multiplicado, así que la matriz resultante se va a tener que calcular siempre como triangular superior
    !Además se calcula el determinante gracias a los elementos diagonales
    use mod_clreal
    implicit none
    integer,intent(in)::n
    real(clreal),intent(out)::deter
    real(clreal),intent(inout)::A(n,n),b(n)

    integer :: i,j,k
    real(clreal) :: z, eps

    eps = 1.e-12
    deter = 1.0

    do k=1,n-1
        if ( abs(a(k,k)) < eps ) then
            stop "Elemento diagonal nulo"
        end if
        do i=k+1,n !Iteramos por filas
            z=a(i,k)/a(k,k) !z va a ser el factor de multiplicación

            !do j=k+1,n !Iteramos por columnas
            !    a(i,j)=a(i,j)-z*a(k,j) !Se trata de la trasformación Fila i = Fila i - z * Fila k
            !enddo
            
            A(i,k+1:n)=A(i,k+1:n)-z*A(k,k+1:n) !Así se hace más facil la transformación
            b(i)=b(i)-z*b(k) !También tenemos que hacer lo mismo con el vector de términos independientes
            a(i,k)=z !Guardamos el factor de multiplicación. Por si acaso. Total, la otra opción era ponerlo a 0.
        enddo
        deter = deter * a(k,k)
    enddo
    deter = deter * a(n,n)
end subroutine gauss