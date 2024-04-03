---
date: 2024-01-26 04:44:57.310747-07:00
description: "C\xF3mo hacerlo: Ruby facilita el manejo de los n\xFAmeros complejos.\
  \ Puedes crearlos y manipularlos usando la clase Complex."
lastmod: '2024-03-13T22:44:59.583695-06:00'
model: gpt-4-0125-preview
summary: "Ruby facilita el manejo de los n\xFAmeros complejos."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
Ruby facilita el manejo de los números complejos. Puedes crearlos y manipularlos usando la clase Complex:

```ruby
require 'complex'

# Crear números complejos
c1 = Complex(3, 4)
c2 = Complex('2+5i')

# Operaciones básicas
suma = c1 + c2               # => (5.0+9.0i)
diferencia = c1 - c2         # => (1.0-1.0i)
producto = c1 * c2           # => (-14.0+23.0i)
cociente = c1 / c2           # => (0.896551724137931+0.03448275862068961i)

# Conjugado, magnitud y fase
conjugado = c1.conjugate     # => (3.0-4.0i)
magnitud = c1.abs            # => 5.0
fase = c1.phase              # Math.atan2(4, 3) => 0.9272952180016122 radianes

# Métodos específicos de complejos
polar = c1.polar             # => [5.0, 0.9272952180016122]
rectangular = c1.rect        # => [3.0, 4.0]
```

## Profundización
Los números complejos no son nuevos—existen desde el siglo XVI, resolviendo ecuaciones sin soluciones reales. Aparte de la matemática, computacionalmente, la clase Complex de Ruby hace el trabajo pesado, con el respaldo del módulo Math para funciones trigonométricas y trascendentales.

Los lenguajes de programación antiguos requerían un manejo manual de las partes real e imaginaria. Algunos, como Fortran y C++, dedican bibliotecas especiales para la aritmética compleja.

El enfoque de Ruby integra el soporte de números complejos en su sintaxis, liberándote de reinventar la rueda. Detrás de escena, la clase Complex se encarga de las matemáticas, mientras que Ruby se ocupa de las interacciones de objetos.

## Ver También
- Documentación de Ruby sobre Complex: [https://ruby-doc.org/core/Complex.html](https://ruby-doc.org/core/Complex.html)
- La perspectiva de MathWorld sobre los Números Complejos: [http://mathworld.wolfram.com/ComplexNumber.html](http://mathworld.wolfram.com/ComplexNumber.html)
- Una introducción visual a los números complejos y por qué son útiles: [https://www.youtube.com/watch?v=5PcpBw5Hbwo](https://www.youtube.com/watch?v=5PcpBw5Hbwo)
