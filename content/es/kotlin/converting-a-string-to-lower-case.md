---
title:                "Convirtiendo una cadena a minúsculas"
html_title:           "Bash: Convirtiendo una cadena a minúsculas"
simple_title:         "Convirtiendo una cadena a minúsculas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## ¿Qué & Por qué?

Convertir una cadena a minúsculas significa cambiar todas las letras mayúsculas a minúsculas. Los programadores lo hacen para normalizar los datos de entrada o para comparaciones de cadenas insensibles a mayúsculas y minúsculas.

## ¿Cómo se hace?

Aquí te muestro un ejemplo sencillo en Kotlin para convertir una cadena a minúscula:

```Kotlin
fun main() {
    val cadenaOriginal = "KOTLIN Es Muy Guay!"
    val cadenaEnMinusculas = cadenaOriginal.lowercase()

    println(cadenaEnMinusculas)
}
```

En este código, el método `lowercase()` de Kotlin se usa para convertir la cadena `cadenaOriginal` a minúsculas.

Al ejecutar el código, se imprimirá el siguiente resultado:

```Kotlin
kotlin es muy guay!
```

## Profundización

Históricamente, la conversión de cadenas a minúsculas ha sido utilizada en todo, desde la normalización de datos hasta la mejora de la usabilidad en las interfaces de usuario. En Kotlin, es un procedimiento sencillo gracias al método integrado `lowercase()`.

En cuanto a alternativas, antes se utilizaba el método `toLowerCase()`, que podría requerir un `Locale` como parámetro para manejar casos especiales. Sin embargo, a partir de Kotlin 1.5, `toLowerCase()` ha sido reemplazado por `lowercase()`.

El `lowercase()` examina cada carácter de la cadena y si es una letra mayúscula, la convierte a minúsculas. Es importante tener en cuenta que este método hace una copia de la cadena y no modifica la cadena original.

## Ver También

Para aprender más sobre la conversión de cadenas a minúsculas en Kotlin, visita las siguientes fuentes:

- Documentación oficial de Kotlin: [Funciones de cadenas y caracteres](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/lowercase.html)
- Guía de JetBrains: [Trabajar con cadenas en Kotlin](https://www.jetbrains.com/help/idea/working-with-kotlin-string-literals.html)