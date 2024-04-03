---
date: 2024-01-26 03:44:44.321885-07:00
description: "Redondear n\xFAmeros es ajustar un decimal a su valor entero m\xE1s\
  \ cercano o a un n\xFAmero especificado de d\xEDgitos fraccionarios. Los programadores\
  \ redondean\u2026"
lastmod: '2024-03-13T22:44:58.974250-06:00'
model: gpt-4-0125-preview
summary: "Redondear n\xFAmeros es ajustar un decimal a su valor entero m\xE1s cercano\
  \ o a un n\xFAmero especificado de d\xEDgitos fraccionarios."
title: "Redondeo de n\xFAmeros"
weight: 13
---

## Cómo hacerlo:
El módulo `Basics` de Elm proporciona funciones útiles para redondear: `round`, `floor` y `ceiling`. Así es como se utilizan.

```elm
import Basics exposing (round, floor, ceiling)

-- Redondear al número entero más cercano
round 3.14    --> 3
round 3.5     --> 4

-- Redondear hacia abajo
floor 3.999   --> 3

-- Redondear hacia arriba
ceiling 3.001 --> 4

-- Truncar decimales sin redondear
truncate 3.76 --> 3
```

Elm también proporciona `toLocaleString` para redondear a un número fijo de lugares decimales:

```elm
import Float exposing (toLocaleString)

-- Redondear a dos lugares decimales
toLocaleString 2 3.14159 --> "3.14"
```

## Profundizando
Elm es un lenguaje funcional de tipado fuerte que relega los efectos secundarios a los "bordes" de la arquitectura. Esto significa que funciones como el redondeo deben ser puras y predecibles. Históricamente, el redondeo es una operación común en muchos lenguajes de programación que tratan con la imprecisión de la aritmética de punto flotante.

El enfoque de Elm para el redondeo es directo: las funciones son puras y se adhieren a las definiciones matemáticas para redondear, floor y ceiling. Elm anticipa las necesidades comunes proporcionando funciones integradas, ya que la gestión de la precisión es un requisito frecuente, especialmente en finanzas y gráficos.

Las alternativas a las funciones integradas de Elm podrían incluir implementaciones personalizadas usando operaciones aritméticas, pero eso añade complejidad innecesaria cuando la biblioteca estándar ya realiza el trabajo de manera eficiente.

Hasta la versión actual, Elm utiliza la matemática de punto flotante subyacente de JavaScript para estas operaciones, manteniéndose así consistente con el estándar IEEE 754, lo cual es algo a tener en cuenta al considerar la precisión y los posibles errores de punto flotante.

## Ver también
- Documentación oficial del módulo `Basics` de Elm: https://package.elm-lang.org/packages/elm/core/latest/Basics
- Una mirada detallada a cómo funcionan los números de punto flotante en la informática: https://floating-point-gui.de/
- Módulo `Float` de Elm para más operaciones de punto flotante: https://package.elm-lang.org/packages/elm/core/latest/Float
