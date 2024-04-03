---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:35.098660-07:00
description: "C\xF3mo: En Kotlin, las cadenas pueden capitalizarse utilizando las\
  \ funciones de la biblioteca est\xE1ndar sin la necesidad de librer\xEDas de terceros.\
  \ El enfoque\u2026"
lastmod: '2024-03-13T22:44:59.018895-06:00'
model: gpt-4-0125-preview
summary: "En Kotlin, las cadenas pueden capitalizarse utilizando las funciones de\
  \ la biblioteca est\xE1ndar sin la necesidad de librer\xEDas de terceros."
title: Capitalizando una cadena de texto
weight: 2
---

## Cómo:
En Kotlin, las cadenas pueden capitalizarse utilizando las funciones de la biblioteca estándar sin la necesidad de librerías de terceros. El enfoque de Kotlin para manejar cadenas hace que estas operaciones sean directas y concisas.

### Capitalizando toda la cadena:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Salida: HELLO, WORLD!
```

### Capitalizando solo el primer carácter:
A partir de Kotlin 1.5, la función `capitalize()` está obsoleta y reemplazada con una combinación de `replaceFirstChar` y una lambda que verifica si es una letra minúscula para transformarla a mayúsculas.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Salida: Hello, world!
```

Este enfoque mantiene el resto de la frase en su forma original mientras solo cambia la primera letra a mayúsculas.
