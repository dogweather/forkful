---
title:    "Haskell: Comparando dos fechas"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

##Por qué

Comparar dos fechas puede tener varias aplicaciones en la programación, como por ejemplo, ordenar eventos en una aplicación de calendario o determinar si una fecha es anterior o posterior a otra para realizar acciones específicas.

##Cómo hacerlo

Hay varias formas de comparar dos fechas en Haskell, pero en este blog post vamos a usar el módulo "Data.Time" y la función "diffDays" que nos permite calcular la diferencia en días entre dos fechas.

``` Haskell
import Data.Time

-- Definimos dos fechas como ejemplos
fecha1 = fromGregorian 2021 07 15
fecha2 = fromGregorian 2021 07 20

-- Calculamos la diferencia en días entre las dos fechas
diferenciaEnDias = diffDays fecha2 fecha1

-- Mostramos el resultado por consola
putStrLn $ "La diferencia en días entre la fecha 1 y la fecha 2 es: " ++ show diferenciaEnDias

-- Output: La diferencia en días entre la fecha 1 y la fecha 2 es: 5
```

Podemos ver que el resultado de la función "diffDays" es un valor entero, lo que nos permite realizar comparaciones lógicas entre fechas en nuestros programas.

##Profundizando

En el ejemplo anterior, usamos la función "fromGregorian" para definir nuestras fechas, pero también podemos crear fechas a partir de un "Day" tipo de datos, que representa un día específico a partir del 1 de enero de 0001.

Además, el módulo "Data.Time" proporciona otras funciones útiles para trabajar con fechas, como "addDays" para agregar un número específico de días a una fecha y "isLeapYear" para determinar si un año es bisiesto.

##Ver también

- Documentación del módulo Data.Time: https://hackage.haskell.org/package/time/docs/Data-Time.html
- Tutorial de Haskell: https://www.haskell.org/tutorial/