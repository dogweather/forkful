---
title:                "Capitalizando una cadena de texto"
date:                  2024-01-19
html_title:           "Arduino: Capitalizando una cadena de texto"
simple_title:         "Capitalizando una cadena de texto"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?
Capitalizar una cadena significa convertir la primera letra de cada palabra a mayúscula. Los programadores lo hacen para normalizar la presentación de textos, títulos o para cumplir con ciertas reglas gramaticales.

## Cómo hacerlo:
```kotlin
fun main() {
    val texto = "esto es un ejemplo"
    val textoCapitalizado = texto.split(" ").joinToString(" ") { it.capitalize() }
    println(textoCapitalizado) // Salida: Esto Es Un Ejemplo
}
```

```kotlin
// En Kotlin 1.5 o superior, "capitalize()" ha sido reemplazado por "replaceFirstChar".
fun main() {
    val texto = "otra cadena de ejemplo"
    val textoCapitalizado = texto.replaceFirstChar { if (it.isLowerCase()) it.titlecase() else it.toString() }
    println(textoCapitalizado) // Salida: Otra cadena de ejemplo
}
```

## Profundizando
Originalmente, Kotlin incluía funciones como `capitalize()` para cambiar la primera letra de una cadena a una mayúscula. En versiones recientes, como en Kotlin 1.4 y posteriores, `capitalize()` está marcada como obsoleta debido a problemas de localización y consistencia. 

Como alternativa, `replaceFirstChar()` se recomienda por ser más explicita sobre la intención de cambiar sólo el primer carácter. Esta función permite la manipulación más flexible de cadenas, incluyendo capitalizar teniendo en cuenta las reglas específicas de un idioma al pasar una función como `{ it.titlecase(Locale) }`.

La implementación al trabajar con la capitalización de cadenas en Kotlin es directa. Sin embargo, los desarrolladores deben tener en cuenta las peculiaridades del idioma y las reglas de mayúsculas y minúsculas asociadas, lo que puede aumentar la complejidad al tratar con localizaciones o soporte de idiomas múltiples.

## Ver También
- [Kotlin Standard Library Documentation](https://kotlinlang.org/api/latest/jvm/stdlib/)
- [Using replaceFirstChar in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)
- [Unicode Character Database](https://unicode.org/ucd/) - para entender cómo trabajar con diferentes normas de caracteres.
- [Kotlin and the Java Locale Class](https://docs.oracle.com/javase/8/docs/api/java/util/Locale.html) - para más detalles sobre cómo trabajar con Locale en Kotlin y Java.
