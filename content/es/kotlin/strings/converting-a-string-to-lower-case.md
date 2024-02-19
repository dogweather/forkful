---
aliases:
- /es/kotlin/converting-a-string-to-lower-case/
date: 2024-01-20 17:38:43.834640-07:00
description: "Convertir una cadena a min\xFAsculas transforma todos los caracteres\
  \ alfab\xE9ticos en su equivalente en min\xFAsculas. Esto es \xFAtil para uniformizar\
  \ datos,\u2026"
lastmod: 2024-02-18 23:09:09.922053
model: gpt-4-1106-preview
summary: "Convertir una cadena a min\xFAsculas transforma todos los caracteres alfab\xE9\
  ticos en su equivalente en min\xFAsculas. Esto es \xFAtil para uniformizar datos,\u2026"
title: "Conversi\xF3n de una cadena de texto a min\xFAsculas"
---

{{< edit_this_page >}}

## What & Why?
Convertir una cadena a minúsculas transforma todos los caracteres alfabéticos en su equivalente en minúsculas. Esto es útil para uniformizar datos, realizar comparaciones insensibles a mayúsculas y simplificar la entrada de texto del usuario.

## How to:
En Kotlin, puedes convertir una cadena a minúsculas con el método `toLowerCase()`. Veamos cómo usarlo:

```kotlin
fun main() {
    val original = "¡Hola, Programador!"
    val enMinusculas = original.lowercase()
    
    println(enMinusculas)  // Imprime: "¡hola, programador!"
}
```

`lowercase()` también considera reglas de localización. Por ejemplo:

```kotlin
import java.util.Locale

fun main() {
    val cadenaConTurco = "PROGRAMACIÓN İ"
    val turcoMinusculas = cadenaConTurco.lowercase(Locale.forLanguageTag("tr"))

    println(turcoMinusculas)  // Imprime: "programación i"
}
```

El carácter 'İ' en turco se convierte a 'i' sin punto, manteniendo la precisión lingüística.

## Deep Dive
Antes de Kotlin 1.5, los métodos eran `toLowerCase()` y `toUpperCase()`. Estos métodos todavía existen pero están marcados como obsoletos y se recomienda usar `lowercase()` y `uppercase()` que son más seguros en relación a la localización de idiomas.

Alternativas al método `lowercase()` podrían ser manipular el código char de cada carácter, pero eso es más complejo y propenso a errores. Además, Kotlin trabaja en Unicode, por lo que `lowercase()` es confiable y maneja excepciones lingüísticas.

La implementación tiene en cuenta el estándar Unicode para la conversión de caracteres, y al utilizar `lowercase(Locale)` puedes especificar la localización para caracteres especiales.

## See Also
Para una guía más amplia sobre cadenas en Kotlin, puedes consultar:

- Documentación oficial de Kotlin sobre cadenas: [Kotlin Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- Información sobre Unicode y localización en conversiones de texto: [Unicode Case Operations](http://userguide.icu-project.org/transforms/case)
