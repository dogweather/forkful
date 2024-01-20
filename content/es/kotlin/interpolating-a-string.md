---
title:                "Interpolando una cadena de texto"
html_title:           "Haskell: Interpolando una cadena de texto"
simple_title:         "Interpolando una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

La interpolación de cadenas es un proceso por el cual inyectamos variables directamente dentro de una cadena. Los programadores lo hacen para simplificar la concatenación de cadenas y hacer el código más legible.

## ¿Cómo se hace?

Aquí se muestra un ejemplo simple de interpolación de cadenas en Kotlin. Observe cómo las variables se incluyen directamente en la cadena.

```Kotlin
val nombre = "Juan"
val saludo = "¡Hola, $nombre!"
println(saludo) // imprimirá: ¡Hola, Juan!
```

Si deseas incluir una expresión más compleja, debes usar llaves `{}` alrededor de ella, así:

```Kotlin
val horas = 7
val mensaje = "Han pasado ${horas * 60} minutos desde la medianoche"
println(mensaje) // imprimirá: Han pasado 420 minutos desde la medianoche
```

## Más allá

La interpolación de cadenas, aunque más comúnmente usada en lenguajes modernos como Kotlin, no es un concepto nuevo. Se originó en lenguajes de programación como Perl y Python hace mucho tiempo.

Ten en mente que hay otras formas de manejar la concatenación de cadenas en Kotlin, como el uso de la función `plus()` o el operador `+`

```Kotlin
val str1 = "Hola, "
val str2 = "Juan"
println(str1.plus(str2)) // imprimirá: Hola, Juan
println(str1 + str2) // imprimirá: Hola, Juan
```
Pero la interpolación de cadenas es mucho más legible y preferida en la mayoría de los casos.

## Ver más

Si deseas seguir aprendiendo sobre Kotlin y la interpolación de cadena, aquí puedes visitar estos enlaces:
