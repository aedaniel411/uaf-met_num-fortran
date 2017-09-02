program bisec
   mplicit none
   double precision :: a, b, TOL, pi, FA, p, FP
   integer :: n0, i
   logical :: ex

   pi = 4.0D0 * atan(1.0D0)
   ex = .false.
   !entrada; extremos (a,b) tolerancia "TOL", numero maximo de iteraciones "N0"
   a = (-1.0D0) * pi / 4.0D0
   b = pi / 4.0D0
   TOL = 1.0D0 / 100.0D0
   n0 = 4
   !paso1; tome i=1, FA=f(a)
   i = 1
   FA = f(a)

   !paso2; mientras i<=N0 haga paso 3-6
   do while (i <= n0)
      !paso3; tome p=a+(b-a)/2; calcule pi,FP=f(p)
      p = a + (b - a) / 2.0D0
      FP = f(p)
      !paso4; si FP=0 o (b-a)/2<tTOL entonces; salida (p) (procedimiento terminado satisfactoriamente)
      if ((FP == 0) .or. ( (b-a) / 2.0 < TOL )) then
         print *, "sastisfactorio", p, FP
         ex = .true.
         exit
      end if
      !paso5;tome i=i+1
      i = i + 1
      !paso6;si FA * FP>0 entonces tome a=p: (calcule a1,b1);
      !FA=FP si no tome b=p (FA no cambia)¡
      if ((FA * FP) > 0) then
         a = p
         FA = f(a)
      else
         b = p
      end if
   end do
   !paso7;salida (el metodo fracaso despues de N0 iteracione, N0=',N0),
   !(procedimiento terminado sin exito) PARAR
   if (.not. (ex)) then
      print *, "sin exito"
   end if
stop
contains

   double precision function f(x)
      double precision :: n,x
         f = x-((x)**2/120.0)-((x)**3/120.0)+(x**7)/5040.0
   end function

end program
