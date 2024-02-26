---
date: 2024-01-26 04:39:38.640596-07:00
description: "Los n\xFAmeros complejos son una combinaci\xF3n de n\xFAmeros reales\
  \ y n\xFAmeros imaginarios, como `a + bi` donde `i` es la ra\xEDz cuadrada de -1.\
  \ Son clave en campos\u2026"
lastmod: '2024-02-25T18:49:55.461667-07:00'
model: gpt-4-0125-preview
summary: "Los n\xFAmeros complejos son una combinaci\xF3n de n\xFAmeros reales y n\xFA\
  meros imaginarios, como `a + bi` donde `i` es la ra\xEDz cuadrada de -1. Son clave\
  \ en campos\u2026"
title: "Trabajando con n\xFAmeros complejos"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Los números complejos son una combinación de números reales y números imaginarios, como `a + bi` donde `i` es la raíz cuadrada de -1. Son clave en campos como la ingeniería y la física para resolver problemas que los números regulares no pueden abordar.

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
