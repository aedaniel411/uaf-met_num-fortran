program neville
   implicit none
   !Entrada:
   !  números de x, x0, x1, x2, ... ,xn
   !              f(x0), f(x1), f(x2), ..., f(xn) como la primera columna de Q
   real :: x, xi(0:9), fi(0:9), Q(0:9,0:9), Px
   integer :: i, j, n

   print *, "¿Valor de x?"
   read (*,*) x

   print *, "¿cuántos puntos?"
   read (*,*) n

   do i = 0, n - 1
      print '(A,I1,A,$)', "Valor de X(",i,"):"
      read (*,*) xi(i)
      print '(A,I1,A,$)', "Valor de F(",i,"):"
      read (*,*) fi(i)
      Q(i,0) = fi(i)
   end do

   !Salida:
   !  tabla de Q con P(x) = Qn,n
   !
   !Paso1:
   !para i=1,2,3,...,n
   do i = 1, n - 1
      !para j=1,2,3,...,i
      do j = 1, i
         !Qi,j=( (x-xi-j)Qi,j-1 - (x - xi)Qi-1,j-1 ) / ( xi - xi-j )
         Q(i,j)=((x-xi(i-j))*Q(i,j-1)-(x-xi(i))*Q(i-1,j-1))/(xi(i)-xi(i-j))
         print '(f10.7,$)', Q(i,j)
      end do
      print *
   end do
   !Salida Q
   Px = Q(n-1,n-1)

   print *, "P(x) =", Px
end program
