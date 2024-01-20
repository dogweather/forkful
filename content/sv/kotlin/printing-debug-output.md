---
title:                "Skriva ut felsökningsresultat"
html_title:           "Fish Shell: Skriva ut felsökningsresultat"
simple_title:         "Skriva ut felsökningsresultat"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skriva ut debugginformation är en process där programmerare skriver ut data från koden för felsökning. Det används för att få insikt i din kods beteende och för att hjälpa till med att identifiera och lösa problem.

## Hur gör man:

Här är några exempel hur du skriver ut debuginformation i Kotlin med kommentarer.

```kotlin
// Skriv ut text med println
println("Hello, World")

// Skriv ut värden av variabler
val name = "Kotlin"
println("Hej, $name")

// Skriv ut resultaten av en funktion
fun addition(a: Int, b: Int) = a + b
println("Summan av 1 + 2 är ${addition(1, 2)}")
```

När du kompilerar och kör ovanstående exempel, får du följande output:

```
Hello, World
Hej, Kotlin
Summan av 1 + 2 är 3
```

## Djupdykning

Historiskt sett, före tillgången till avancerade debuggers och IDE:er, har utskrift av debugginformation varit ett viktigt verktyg för att förstå vad som hände i en applikation. De kallades ofta "printf-debugging", efter printf-funktionen i C.

Ett alternativ till utskrift av debugginformation är att använda en debugger. En debugger ger mer sofistikerade möjligheter att inspektera din kod, men i enklare fall kan det vara snabbare och enklare att bara skriva ut informationen.

I Kotlin, metoden `println()` är en inbyggd standardkod för att skriva ut information till terminalen. Den kan hantera alla former av variabler och funktioner genom att konvertera dem till teckensträngar före utskrift.

## Se också

- Kotlin Dokumentationen om [println()](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/println.html)