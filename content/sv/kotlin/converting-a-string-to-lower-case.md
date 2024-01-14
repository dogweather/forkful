---
title:                "Kotlin: Konvertera en sträng till gemener"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera en sträng till små bokstäver är en vanlig operation inom programmering. Det kan användas för att jämföra eller söka igenom strängar utan att behöva bry sig om stor eller liten bokstav. Det är också användbart för att formatera data eller för att ge användare möjlighet att skriva in data utan att behöva bry sig om bokstavsstorlek.

## Så här gör du

För att konvertera en sträng till små bokstäver kan du använda Kotlin-funktionen ```toLowerCase()```. Här är ett enkelt exempel:

```Kotlin
val sträng = "tHIs IS a StRInG"
val konverteradSträng = sträng.toLowerCase()
println(konverteradSträng) // output: this is a string
```

Det här är en grundläggande metod för att konvertera en hel sträng till små bokstäver. Men det finns också andra sätt som är mer flexibla och tillåter användaren att välja vilka delar av strängen som ska konverteras.

## Djupdykning

När du använder funktionen ```toLowerCase()``` på en sträng i Kotlin, skapas en ny sträng där alla bokstäver har konverterats till små. Detta kan vara en ineffektiv metod om du bara behöver konvertera vissa delar av en stor sträng. För att undvika att skapa en helt ny sträng kan du använda en stream och en lambda-funktion för att konvertera specifika delar av strängen. Här är ett exempel:

```Kotlin
val sträng = "tHIs IS a StRInG"
val konverteradSträng = sträng
    .stream()
    .map { bokstav -> bokstav.toLowerCase() } // konverterar alla bokstäver i streamen
    .collect(Collectors.joining()) // slår ihop bokstäverna till en sträng igen
println(konverteradSträng) // output: this is a string
```

Detta är en mer flexibel metod och kan vara användbar när du behöver konvertera delar av en stor sträng.

## Se även

- [Kotlin String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Kotlin Stream documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/stream.html)
- [Java String toLowerCase() documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())