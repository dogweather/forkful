---
title:                "Eliminando caracteres que coinciden con un patrón"
date:                  2024-01-20T17:42:43.450842-07:00
model:                 gpt-4-1106-preview
simple_title:         "Eliminando caracteres que coinciden con un patrón"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?

Eliminar caracteres que coinciden con un patrón es básicamente filtrar una cadena de texto, manteniendo solo los caracteres que queremos. Los programadores lo hacen para validar la entrada, limpiar datos o simplificar el procesamiento de texto.

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
