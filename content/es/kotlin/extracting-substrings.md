---
title:                "Extrayendo subcadenas"
html_title:           "C: Extrayendo subcadenas"
simple_title:         "Extrayendo subcadenas"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

Extraer substrings es separar una parte de una cadena existente. Los programadores lo hacen para analizar datos, redistribuir piezas de texto y manipular cadenas.

## ¿Cómo se hace?

En Kotlin, tenemos la función `substring`. Aquí están los ejemplos:

```Kotlin
val texto = "Hola, Mundo Kotlin"
val substring = texto.substring(0, 4)  // "Hola"
```

La función `substring` toma el inicio y el fin del substring que queremos. Devuelve el substring entre estos índices.  

Otra opción es usar rangos:

```Kotlin
val substring = texto.substring(0..4)  // "Hola,"
```

## Detalles Adicionales

Kotlin adopta el enfoque de otras lenguajes como JavaScript y Java para extraer substrings, facilitando a los desarrolladores aprender y aplicar este concepto.

Aunque la función `substring` es ampliamente utilizada, podrías usar la expresión regular si los escenarios son complejos. La biblioteca de Kotlin también suministra la función `slice` que toma un rango como parámetro.

Para el rendimiento, cuando extraes un substring, Kotlin no crea un nuevo objeto de cadena, sino que simplemente apunta a la cadena original y usa un par de índices para indicar el inicio y el fin del substring. Esto hace que la operación de extracción sea extremadamente eficiente.

## Ver También

1. La documentación oficial de Kotlin sobre el [trabajo con cadenas](https://kotlinlang.org/docs/idioms.html#working-with-collections)
3. Para los nuevos en Kotlin, comienza con el [Tutorial oficial de Kotlin para principiantes](https://kotlinlang.org/docs/tutorials/)