---
date: 2024-01-26 04:39:38.640596-07:00
description: "C\xF3mo hacerlo: Elm no tiene soporte incorporado para n\xFAmeros complejos,\
  \ as\xED que crear\xE1s tu propio tipo y funciones. Aqu\xED hay una configuraci\xF3\
  n r\xE1pida."
lastmod: '2024-03-13T22:44:58.973321-06:00'
model: gpt-4-0125-preview
summary: "Elm no tiene soporte incorporado para n\xFAmeros complejos, as\xED que crear\xE1\
  s tu propio tipo y funciones."
title: "Trabajando con n\xFAmeros complejos"
weight: 14
---

## Cómo hacerlo:
Elm no tiene soporte incorporado para números complejos, así que crearás tu propio tipo y funciones. Aquí hay una configuración rápida:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Ejemplo de uso:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum es { real = 4.0, imaginary = -2.0 }
```

## Análisis Profundo
Históricamente, los números complejos no siempre fueron aceptados. Se convirtieron en un cambio de juego en el siglo XVI para resolver ecuaciones cúbicas. Alternativas en otros lenguajes como Python ofrecen soporte incorporado para números complejos con operaciones listas para usar. Elm requiere un enfoque de hágalo usted mismo como has visto. Pero puedes hacerlo tan sofisticado como sea necesario, construyendo multiplicación, división y otras operaciones, afinando problemas de rendimiento.

## Ver También
- Documentación Oficial de Elm: https://package.elm-lang.org/ para crear tipos personalizados y dominar los conceptos básicos de Elm.
- Los aficionados a la historia de la matemática podrían consultar "An Imaginary Tale" por Paul J. Nahin para un viaje a través del tiempo de los números complejos.
- Sumérgete en desafíos de programación orientados a la matemática en Project Euler (https://projecteuler.net) para aplicar tu magia con números complejos.
