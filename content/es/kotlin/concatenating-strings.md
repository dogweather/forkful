---
title:                "Concatenando cadenas de texto"
html_title:           "Arduino: Concatenando cadenas de texto"
simple_title:         "Concatenando cadenas de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## ¿Qué & Por Qué?

La concatenación de cadenas es simplemente juntar dos o más cadenas. Los programadores la usan cuando necesitan unir o combinar información en forma de texto.

## Cómo hacerlo:

Vamos a mostrar dos métodos comunes para concatenar cadenas en Kotlin.

1. Usando el operador `+`:

```Kotlin
fun main() {
    val str1 = "¡Hola,"
    val str2 = " Mundo!"
    val resultado = str1 + str2
    println(resultado)
}
```
Este código imprimirá: `¡Hola, Mundo!`

2. Usando la función `concat`:

```Kotlin
fun main() {
    val str1 = "¡Hola,"
    val str2 = " Mundo!"
    val resultado = str1.concat(str2)
    println(resultado)
}
```
Esto también imprimirá: `¡Hola, Mundo!`

## Análisis en Profundidad:

1. *Historia* - La concatenación de cadenas es una de las operaciones más antigua en los lenguajes de programación, introducida con los primeros lenguajes como COBOL y FORTRAN.

2. *Alternativas* - En algunos casos, puedes usar la interpolación de cadenas en lugar de la concatenación. Esta es una característica que permite incrustar valores de variables directamente en cadenas. Por ejemplo:

```Kotlin
val nombre = "Mundo"
val saludo = "¡Hola, $nombre!"
println(saludo)
```
3. *Detalles de implementación* - El operador `+` está sobrecargado en Kotlin para realizar la concatenación de cadenas. Sin embargo, ten en cuenta que cada uso de `+` crea una nueva cadena, lo que puede ser ineficiente en bucles con muchas iteraciones.

## Ver También:

- Kotlin Docs sobre cadenas: [https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- Implementando interpolación de cadenas en Kotlin: [https://kotlinlang.org/docs/languages.html#string-templates](https://kotlinlang.org/docs/languages.html#string-templates)