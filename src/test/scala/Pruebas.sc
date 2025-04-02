import ManiobrasTrenes._

// Pruebas para aplicarMovimiento
val e1 = (List('a', 'b', 'c', 'd'), Nil, Nil)
val e2 = aplicarMovimiento(e1, Uno(2))
val e3 = aplicarMovimiento(e2, Dos(3))
val e4 = aplicarMovimiento(e3, Dos(-1))
val e5 = aplicarMovimiento(e4, Uno(-2))

// Pruebas para aplicarMovimientos
aplicarMovimientos(e1, List(Uno(2), Dos(3), Dos(-1), Uno(-2), Dos(-1)))

val e = (List('a', 'b'), List('c'), List('d'))
aplicarMovimientos(e, List(Uno(1), Dos(1), Uno(-2)))

// Prueba para definirManiobra
definirManiobra(List('a', 'b', 'c', 'd'), List('d', 'b', 'c', 'a'))

// Pruebas adicionales para aplicarMovimiento
val test1 = (List(1, 2, 3, 4, 5), List(), List())
val res1 = aplicarMovimiento(test1, Uno(3))
// Debería ser (List(1, 2), List(3, 4, 5), List())

val test2 = (List(1), List(2, 3), List(4, 5))
val res2 = aplicarMovimiento(test2, Uno(-1))
// Debería ser (List(1, 2), List(3), List(4, 5))

val test3 = (List(), List(1, 2, 3), List(4, 5))
val res3 = aplicarMovimiento(test3, Dos(-2))
// Debería ser (List(4, 5), List(1, 2, 3), List())

val test4 = (List(1, 2, 3), List(4, 5), List())
val res4 = aplicarMovimiento(test4, Dos(3))
// Debería ser (List(), List(4, 5), List(1, 2, 3))

val test5 = (List(1, 2), List(3, 4), List(5, 6))
val res5 = aplicarMovimiento(test5, Uno(0))
// Debería mantener el mismo estado

// Pruebas adicionales para aplicarMovimientos
val test6 = (List(1, 2, 3, 4), List(), List())
val res6 = aplicarMovimientos(test6, List(Uno(2), Dos(1), Uno(-1), Dos(-1)))
// Debería mostrar la transición de estados correcta

val test7 = (List('x', 'y', 'z'), List(), List())
val res7 = aplicarMovimientos(test7, List(Uno(1), Dos(1), Uno(1), Dos(-1), Uno(-1)))
// Debería mostrar la transición de estados correcta

// Pruebas adicionales para definirManiobra
val res8 = definirManiobra(List(1, 2, 3), List(3, 2, 1))
// Debería generar una secuencia válida de movimientos para invertir el orden

val res9 = definirManiobra(List('a', 'b', 'c'), List('b', 'c', 'a'))
// Debería generar una secuencia válida de movimientos para reorganizar

val res10 = definirManiobra(List('p', 'q', 'r', 's'), List('s', 'r', 'q', 'p'))
// Debería generar una secuencia válida de movimientos para invertir completamente el orden