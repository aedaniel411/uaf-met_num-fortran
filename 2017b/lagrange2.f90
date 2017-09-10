program lagrange2
   implicit none
   integer :: i, n, j, o
   real :: x, xi(0:9), fi(0:9), Li, fn, sum

   print *, "¿Valor de x?"
   read (*,*) x

   print *, "¿cuántos puntos?"
   read (*,*) n

   do i = 0, n - 1
      print '(A,I1,A,$)', "Valor de X(",i,"):"
      read (*,*) xi(i)
      print '(A,I1,A,$)', "Valor de F(",i,"):"
      read (*,*) fi(i)
   end do

   do i = 0, n - 1
      print *, xi(i), fi(i)
   end do

   sum = 0.0
   do o = 2, n
      do i = 0, o - 1

         Li = 1.0
         do j = 0, i
            if (i /= j) then
               Li = Li * (x - xi(j)) / (xi(i) - xi(j))
            end if
         end do
         sum = sum + (Li * fi(i))
      end do
      fn = sum
      print *, o - 1, fn
   end do

end program
