---
title:                "Eliminando caracteres que coinciden con un patrón"
html_title:           "Elixir: Eliminando caracteres que coinciden con un patrón"
simple_title:         "Eliminando caracteres que coinciden con un patrón"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## ¿Qué y Por qué?

Eliminar caracteres que coinciden con una plantilla es buscar y eliminar caracteres específicos en una cadena, basado en un patrón dado. Los programadores hacen esto para limpiar las cadenas de caracteres no deseados o para transformar la cadena en un formato específico.

## Cómo:

Aquí hay un ejemplo de cómo puedes hacer esto en Kotlin:

```Kotlin
fun main() {
  val pattern = "[^a-z]".toRegex()
  val inputString = "C0de P37#ña"
  val outputString = inputString.replace(pattern, "")
  println(outputString)
}
```

El programa anterior, elimina todos los caracteres que no son letras minúsculas de la cadena "C0de P37#ña", y luego imprime "deña". Indicamos al programa que reemplace cualquier cosa que NO sea una letra minúscula con nada.

## Inmersión Profunda:

1. Contexto histórico: Kotlin es un lenguaje de programación moderno que se utiliza mucho para el desarrollo de Android. La capacidad de eliminar caracteres de una cadena en base a un patrón es una característica que ha existido por mucho tiempo en muchos lenguajes de programación.
   
2. Alternativas: Kotlin también permite eliminar caracteres usando métodos de filtrado en lugar de Regex. Aquí hay un ejemplo:
```Kotlin
fun main() {
   val inputString = "C0de P37#ña"
   val outputString = inputString.filter { it.isLowerCase() }
   println(outputString)
}
```
Este programa también imprime "deña".

3. Detalles de implementación: En Kotlin, la función replace utiliza la plantilla regex para buscar en la cadena. Cada vez que encuentra una coincidencia, la reemplaza. Esto sigue hasta que se ha buscado toda la cadena.

## Véase También:

Para más información sobre Regex en Kotlin, consulta la documentación oficial:
- [Kotlin Regular Expressions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)

Para una visión más profunda de la función replace, mira en Kotlin Playground: