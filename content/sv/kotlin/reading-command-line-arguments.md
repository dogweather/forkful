---
title:                "Läsning av kommandoradsargument"
html_title:           "Kotlin: Läsning av kommandoradsargument"
simple_title:         "Läsning av kommandoradsargument"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att läsa in kommandoradsargument betyder att läsa de inmatade argumenten från terminalen när du kör ditt program. Det är en viktig funktion för programmerare eftersom det tillåter ett program att ta emot olika argument och på så sätt skapa mer anpassade och flexibla lösningar.

## Så här gör du:
Kotlin har en inbyggd funktion för att läsa in kommandoradsargument, nämligen ```main(args: Array<String>)```. Här är ett exempel på hur du kan använda den:

```Kotlin
fun main(args: Array<String>) {
  println("Antal argument: ${args.size}")
  println("Argument: ${args.joinToString()}")
}
```
Om vi kör detta program och anger argumenten "Hej" och "Världen" när vi kör det, så får vi följande utmatning:

```
Antal argument: 2
Argument: Hej, Världen
```

## Djupdykning:
Att läsa in kommandoradsargument är en viktig funktion för att kunna skapa mer anpassade program som kan köras med olika parametrar. Det ger också användaren större kontroll över programmet och möjlighet att anpassa det efter deras behov.

Alternativ till att läsa in kommandoradsargument är att använda användarens inmatning via terminalen eller att ha färdigdefinierade variabler i koden. Men båda dessa alternativ är mindre flexibla och kan göra det svårt att anpassa programmet till olika situationer.

Implementationen av kommandoradsargument är relativt enkel i Kotlin tack vare den inbyggda funktionen ```main(args: Array<String>)```. Det är viktigt att notera att argumenten läses in som en array av strängar, så eventuella omvandlingar till andra datatyper måste göras manuellt.

## Se också:
- Kotlin Tutorials: [Reading Command Line Arguments](https://kotlinlang.org/docs/tutorials/command-line.html)
- Baeldung: [Passing Command Line Arguments to a Kotlin App](https://www.baeldung.com/java-command-line-arguments-kotlin)