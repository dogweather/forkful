---
title:                "Utskrift av felsökningsutdata"
html_title:           "Kotlin: Utskrift av felsökningsutdata"
simple_title:         "Utskrift av felsökningsutdata"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utdata är ett viktigt verktyg för att felsöka och förstå vad som händer i din kod. Genom att skriva ut relevant information vid olika steg i koden kan du enklare identifiera problem och förbättra din kod. Det är ett enkelt sätt att få inblick i exakt vad som händer bakom kulisserna.

## Hur man gör

För att skriva ut debug-utdata i Kotlin kan du använda funktionen `println()` eller `print()`. Dessa funktioner skriver ut text utan att lägga till en ny rad efter, vilket gör det bra för att skriva ut variabler eller andra värden, till exempel:

```Kotlin
val nummer = 10
println("Detta är värdet av nummer: $nummer")
```

Detta kommer att skriva ut "Detta är värdet av nummer: 10". Som du kan se används dollartecken och variabelnamnet inom `println()`-funktionen för att skriva ut värdet av variabeln. Detta gör att du kan skriva ut flera variabler eller text i samma `println()`-funktion.

Om du vill lägga till en ny rad efter utskriften kan du använda `println()` med en tom sträng som argument, till exempel:

```Kotlin
val namn = "Anna"
val ålder = 25 
println("Namn: $namn")
println("Ålder: $ålder")
println()
```

Detta kommer att skriva ut:

```
Namn: Anna
Ålder: 25
```

En annan användbar funktion för debug-utdata är `debug()` som finns i `android.util.Log`-klassen. Denna funktion används vanligtvis i Android-utveckling för att logga information till Android Studio's loggverktyg. Du kan till exempel använda den för att logga ett felmeddelande:

```Kotlin
Log.d("TAG", "Ett fel har inträffat.")
```

Där "TAG" är en sträng som identifierar loggmeddelandet och "Ett fel har inträffat." är det faktiska meddelandet som kommer att visas i loggen.

## Fördjupning

Det finns många andra användbara funktioner och metoder som kan hjälpa dig att skriva ut debug-utdata, till exempel `assert()` för att testa förväntade värden och `warn()` för att logga varningar. Du kan också använda `Log.i()` för att logga information och `Log.e()` för att logga felmeddelanden av högre prioritet.

Du kan också använda och formatera strängar med hjälp av `String.format()` för att skapa mer läsbar och strukturerad debug-utdata. Dessutom finns det många tredjepartsbibliotek som kan hjälpa dig att logga, spåra och analysera debug-utdata.

Med alla dessa olika verktyg är det viktigt att vara selektiv med vad du skriver ut. Att ha för mycket debug-utdata kan göra det svårt att hitta viktig information och kan sakta ner din kod. Se till att välja ut relevant information och radera all debug-kod innan du skickar din app till produktion.

## Se även

Här är några användbara resurser för att lära dig mer om debug-utdata med Kotlin:

- [Officiell Kotlin dokumentation för debugging](https://kotlinlang.org/docs/debugging.html)
- [Stack Overflow: How to debug using println](https://stackoverflow.com/a/51116072)
- [Android Developers: Logging](https://developer.android.com/reference/android/util/Log)