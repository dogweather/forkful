---
aliases:
- /sv/kotlin/capitalizing-a-string/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:05:41.449257-07:00
description: "Att kapitalisera en str\xE4ng i programmering inneb\xE4r att konvertera\
  \ det f\xF6rsta tecknet i str\xE4ngen till versal om det inte redan \xE4r det, vilket\
  \ \xE4r anv\xE4ndbart\u2026"
lastmod: 2024-02-18 23:08:51.733751
model: gpt-4-0125-preview
summary: "Att kapitalisera en str\xE4ng i programmering inneb\xE4r att konvertera\
  \ det f\xF6rsta tecknet i str\xE4ngen till versal om det inte redan \xE4r det, vilket\
  \ \xE4r anv\xE4ndbart\u2026"
title: "G\xF6r om en str\xE4ng till versaler"
---

{{< edit_this_page >}}

## Vad & Varför?

Att kapitalisera en sträng i programmering innebär att konvertera det första tecknet i strängen till versal om det inte redan är det, vilket är användbart för att formatera användarinmatningar eller visa text i ett användargränssnitt på ett mer standardiserat eller användarvänligt sätt. Programmerare utför denna operation för att säkerställa datakonsekvens eller för att uppfylla specifika formateringskrav inom deras programvaruapplikationer.

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
