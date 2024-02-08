---
title:                "Extracción de subcadenas"
aliases:
- es/kotlin/extracting-substrings.md
date:                  2024-01-20T17:46:00.519134-07:00
model:                 gpt-4-1106-preview
simple_title:         "Extracción de subcadenas"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Qué y Por Qué?
Extraer subcadenas significa sacar partes específicas de un texto. Los programadores lo hacen para analizar y manipular datos más facilmente.

## Cómo hacerlo:
```kotlin
fun main() {
    val texto = "¡Hola, mundo!"
    
    // Extraer usando índices
    val saludo = texto.substring(0, 5)
    println(saludo) // Imprime: ¡Hola

    // Extraer una subcadena hasta el final
    val mundo = texto.substring(7)
    println(mundo) // Imprime: mundo!

    // Usar la función 'take' y 'drop'
    val exclamacion = texto.takeLast(1)
    println(exclamacion) // Imprime: !

    // Eliminar una parte y quedarse con el resto
    val sinExclamacion = texto.dropLast(1)
    println(sinExclamacion) // Imprime: ¡Hola, mundo
}
```

## Inmersión Profunda
Extraer subcadenas es antiguo como el manejo de cadenas de texto en programación. Kotlin maneja las cadenas de manera inmutable, por lo que al extraer una subcadena, se crea una nueva cadena. Alternativas a `substring` podrían ser `split`, para dividir en varias subcadenas basadas en un delimitador, o expresiones regulares para secuencias más complejas. A nivel de implementación, `substring` en Kotlin se apoya en Java, pero garantiza seguridad al prevenir errores comunes, como el desbordamiento de índice.

## Ver También
- Guía para expresiones regulares en Kotlin: [Regular Expressions in Kotlin](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- Tutorial de Kotlin para principiantes: [Kotlin for Beginners](https://kotlinlang.org/docs/home.html)
