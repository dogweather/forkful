---
title:                "Gör om en sträng till versaler"
aliases:
- /sv/kotlin/capitalizing-a-string.md
date:                  2024-02-03T19:05:41.449257-07:00
model:                 gpt-4-0125-preview
simple_title:         "Gör om en sträng till versaler"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
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
