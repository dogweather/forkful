---
title:                "Comparando dos fechas"
html_title:           "Gleam: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Comparar dos fechas es una tarea común para los programadores. Esto implica tomar dos fechas diferentes y determinar cuál de las dos es mayor, menor o si son iguales. Los programadores suelen hacer esto para ordenar listas de fechas, realizar cálculos de tiempo o para validar fechas de entrada del usuario.

## Cómo:

Comparar dos fechas es muy sencillo en Gleam. Primero, debemos importar el módulo de tiempo ```gleam/time```. Luego, podemos usar la función ```compare``` para comparar dos objetos de fecha. Esta función devuelve un número entero que indica si la primera fecha es mayor, menor o igual a la segunda fecha.

```
Gleam importar/tiempo

let fecha1 = Gleam.tiempo.string_a_fecha("2021-03-15")
let fecha2 = Gleam.tiempo.string_a_fecha("2021-03-16")

Gleam.tiempo.comparar(fecha1, fecha2)  // Devuelve -1
```

En este ejemplo, la primera fecha es menor que la segunda ya que la función ```compare``` devuelve -1. También podemos usar los operadores ```<, >``` y ```==``` para comparar fechas.

## Profundizando:

La comparación de fechas ha sido un problema común en programación desde hace mucho tiempo. Antes de la introducción de tipos de datos nativos de fecha en muchos lenguajes de programación, los programadores tenían que realizar complicados cálculos para comparar fechas. Afortunadamente, en Gleam, este proceso es mucho más sencillo gracias al módulo de tiempo incorporado. Alternativas para comparar fechas incluyen el uso de bibliotecas externas o implementar una solución personalizada.

## Ver También:

- [Documentación del módulo Gleam de tiempo](https://gleam.run/lib/time/)
- [Documentación de la biblioteca de fechas y hora externa Chronos](https://hexdocs.pm/chronos/readme.html)