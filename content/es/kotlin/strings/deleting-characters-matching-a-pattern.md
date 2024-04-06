---
date: 2024-01-20 17:42:43.450842-07:00
description: "C\xF3mo hacerlo: El c\xF3digo elimina todos los d\xEDgitos del `textoOriginal`\
  \ usando una expresi\xF3n regular (el patr\xF3n \"\\\\d\" que encuentra d\xEDgitos)\
  \ y `replace`."
lastmod: '2024-04-05T21:54:00.363398-06:00'
model: gpt-4-1106-preview
summary: "El c\xF3digo elimina todos los d\xEDgitos del `textoOriginal` usando una\
  \ expresi\xF3n regular (el patr\xF3n \"\\\\d\" que encuentra d\xEDgitos) y `replace`."
title: "Eliminando caracteres que coinciden con un patr\xF3n"
weight: 5
---

## Cómo hacerlo:
```kotlin
fun main() {
    val textoOriginal = "¡Hola, programadores123! ¿Listos para programar?"
    val patron = "\\d".toRegex() // Patrón para eliminar dígitos
    val textoLimpio = textoOriginal.replace(patron, "")

    println(textoLimpio) // Muestra: ¡Hola, programadores! ¿Listos para programar?
}
```

El código elimina todos los dígitos del `textoOriginal` usando una expresión regular (el patrón "\\d" que encuentra dígitos) y `replace`.

## Análisis Profundo
Históricamente, el manejo de texto es un componente esencial de la programación. Las primeras computadoras se usaban principalmente para cálculos numéricos, pero rápidamente se empezó a trabajar con texto para aplicaciones más generales. Con el tiempo, las expresiones regulares se convirtieron en una herramienta poderosa para la manipulación de texto, permitiendo búsqueda y reemplazo de patrones complejos. Kotlin incorpora este concepto con clases y funciones específicas en su estándar de bibliotecas.

Alternativas para eliminar caracteres podrían ser el uso de funciones `filter` o `filterNot` para incluir o excluir caracteres específicos sin usar expresiones regulares:

```kotlin
val textoLimpio = textoOriginal.filterNot { it.isDigit() }
```

A nivel de implementación, eliminar caracteres con patrones puede ser más costoso en términos de rendimiento que operaciones más simples debido a la carga de compilar el patrón y aplicar la lógica de la expresión regular a toda la cadena. Es importante considerar esto al trabajar con textos muy largos o en aplicaciones que requieren alta performance.

## Ver También
- [Kotlin Regex class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Kotlin replace function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html)
- [Kotlin filterNot function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/filter-not.html)
- [Regular-Expressions.info](https://www.regular-expressions.info/)
- [Kotlin Playground](https://play.kotlinlang.org/) - para experimentar con código online
