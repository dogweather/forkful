---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:41.449257-07:00
description: "Hur man g\xF6r: I Kotlin kan str\xE4ngar kapitaliseras med hj\xE4lp\
  \ av standardbiblioteksfunktioner utan behov av tredjepartbibliotek. Kotlins tillv\xE4\
  gag\xE5ngss\xE4tt\u2026"
lastmod: '2024-03-13T22:44:37.854825-06:00'
model: gpt-4-0125-preview
summary: "I Kotlin kan str\xE4ngar kapitaliseras med hj\xE4lp av standardbiblioteksfunktioner\
  \ utan behov av tredjepartbibliotek."
title: "G\xF6r om en str\xE4ng till versaler"
weight: 2
---

## Hur man gör:
I Kotlin kan strängar kapitaliseras med hjälp av standardbiblioteksfunktioner utan behov av tredjepartbibliotek. Kotlins tillvägagångssätt för att hantera strängar gör dessa operationer raka och koncisa.

### Kapitalisera hela strängen:
```kotlin
val message = "hello, world!"
val capitalizedMessage = message.uppercase()

println(capitalizedMessage) // Utdata: HELLO, WORLD!
```

### Kapitalisera endast det första tecknet:
Från och med Kotlin 1.5 är funktionen `capitalize()` avskaffad och ersatt med en kombination av `replaceFirstChar` och en lambda som kontrollerar om det är en liten bokstav för att omvandla den till versal.

```kotlin
val greeting = "hello, world!"
val capitalizedGreeting = greeting.replaceFirstChar {
    if (it.isLowerCase()) it.titlecase() else it.toString()
}

println(capitalizedGreeting) // Utdata: Hello, world!
```

Denna metod bibehåller resten av meningen i dess ursprungliga form medan endast den första bokstaven ändras till versal.
