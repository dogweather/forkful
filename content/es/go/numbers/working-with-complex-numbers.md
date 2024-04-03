---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:14:37.247519-07:00
description: "Trabajar con n\xFAmeros complejos en programaci\xF3n implica manipular\
  \ n\xFAmeros que tienen tanto una parte real como una imaginaria, t\xEDpicamente\
  \ expresados como\u2026"
lastmod: '2024-03-13T22:44:58.460488-06:00'
model: gpt-4-0125-preview
summary: "Trabajar con n\xFAmeros complejos en programaci\xF3n implica manipular n\xFA\
  meros que tienen tanto una parte real como una imaginaria, t\xEDpicamente expresados\
  \ como `a + bi`."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo:
En Go, los números complejos se manejan utilizando las funciones integradas `complex`, `real` e `imag`, junto con los tipos `complex64` y `complex128` (que representan números complejos de 64 bits y 128 bits respectivamente). Aquí tienes una guía rápida:

```go
package main

import (
	"fmt"
)

func main() {
	// Creando números complejos
	a := complex(2, 3) // 2+3i
	b := complex(1, -1) // 1-1i

	// Operaciones aritméticas
	c := a + b
	fmt.Println("Suma:", c) // Salida: Suma: (3+2i)

	d := a * b
	fmt.Println("Multiplicación:", d) // Salida: Multiplicación: (5+1i)

	// Accediendo a partes real e imaginaria
	parteReal := real(a)
	parteImaginaria := imag(a)
	fmt.Printf("Parte real: %.1f, Parte imaginaria: %.1f\n", parteReal, parteImaginaria) // Salida: Parte real: 2.0, Parte imaginaria: 3.0

	// Se pueden calcular el conjugado complejo y la magnitud
	conjugado := complex(real(a), -imag(a)) // Manualmente
	fmt.Println("Conjugado de a:", conjugado) // Salida: Conjugado de a: (2-3i)
}

```

Este ejemplo cubre los conceptos básicos, pero hay mucho más que puedes hacer con los números complejos, incluida la utilización del paquete `math/cmplx` para operaciones más avanzadas como encontrar la magnitud, la fase, y mucho más.

## Estudio profundo
El concepto de números complejos se remonta al siglo XVI, pero solo obtuvo un amplio reconocimiento y formalización rigurosa en el siglo XIX. En la programación de computadoras, los números complejos han sido un pilar para el cálculo aritmético complejo en cálculos científicos y de ingeniería desde los primeros días. El enfoque de Go hacia los números complejos, al convertirlos en ciudadanos de primera clase con soporte integrado y un amplio soporte en la biblioteca estándar a través del paquete `math/cmplx`, se destaca entre los lenguajes de programación. Esta decisión de diseño refleja el énfasis de Go en la simplicidad y el rendimiento.

No obstante, vale la pena señalar que trabajar con números complejos en Go, aunque poderoso, puede no ser siempre el enfoque más adecuado para todas las aplicaciones, particularmente aquellas que requieren matemáticas simbólicas o aritmética de alta precisión. Los lenguajes y entornos especializados en la computación científica, como Python con bibliotecas como NumPy y SciPy, o software como MATLAB, podrían ofrecer más flexibilidad y una gama más amplia de funcionalidades para aplicaciones específicas.

Dicho esto, para la programación de sistemas y contextos donde integrar cálculos de números complejos en una aplicación más grande y sensible al rendimiento es crucial, el soporte nativo de Go para números complejos proporciona una opción eficientemente única.
