---
title:                "Comparando dos fechas"
html_title:           "C#: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/comparing-two-dates.md"
---

{{< edit_this_page >}}

-----
# Una Guía Simple para Comparar Fechas en Haskell

## ¿Qué y Por qué?
El comparar dos fechas es un proceso para determinar cuál fecha sucede antes, después o si ambas son iguales. Como programadores, lo hacemos para ordenar eventos, determinar duraciones y gestionar plazos.

## Cómo hacerlo:
Vamos a usar el paquete `time` de Haskell.

Para instalar el paquete time, utiliza este comando en tu terminal:

```Haskell
cabal install time
```

Echemos un vistazo a cómo definiríamos y compararíamos dos fechas:

```Haskell
import Data.Time

fecha1 = fromGregorian 2021 12 28
fecha2 = fromGregorian 2022 1 5

comparacion = compare fecha1 fecha2
```

Si ejecutas este código, verás que `comparacion` será `LT` porque `fecha1` es menor (es decir, anterior) que `fecha2`.

## Inmersión Profunda

**1. Contexto histórico**

El paquete `time` de Haskell fue diseñado conjuntamente con la biblioteca de tiempo POSIX de la Biblioteca de Haskell de la plataforma de Glasgow (GHC) alrededor de 2006-2007.

**2. Alternativas**

Si bien `time` es una excelente elección para la mayoría de los casos, hay otras bibliotecas y métodos para comparar fechas en Haskell, como `clock`, que también puede manejar zonas horarias.

**3. Implementación**

Este código utiliza la función `compare` que es parte del `Ord` tipo de Haskell. Esta función devuelve uno de tres constructores `Ordering`: `LT`, `GT` o `EQ` - que representan "menor que ", "mayor que" y "igual que", respectivamente.

## Ver También

1. Biblioteca `time` de Hackage: [Hackage Library](http://hackage.haskell.org/package/time)
2. Biblioteca `clock` de Hackage: [Hackage Library](http://hackage.haskell.org/package/clock)
3. Una explicación más detallada de `Ord` y comparación: [Real World Haskell](http://book.realworldhaskell.org/read/using-typeclasses.html)
-----