---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/generating-random-numbers.md"
---

{{< edit_this_page >}}

# Slumpmässig nummergenerering i Kotlin
Det här är din grundläggande, snabba guide utan krusiduller till att generera slumpmässiga nummer i Kotlin.

## Vad & Varför?
Att generera slumpmässiga tal är processen för att skapa siffror som inte kan förutses bättre än av en tillfällig chans. Programmerare gör detta för olika ändamål: simuleringar, tester, spel och mer.

## Hur man gör det:
I Kotlin kan vi generera slumpmässiga tal med ett par smidiga metoder. Nedan följer exempel på hur man genererar olika typer av slumpmässiga tal.

För ett slumpmässigt heltal mellan 0 och en angiven gräns:

```Kotlin
val randomInt = Random.nextInt(100)  // Ger ett slumptal mellan 0 och 99
println(randomInt)
```
För ett slumpmässigt flyttal (Double) mellan 0.0 (inklusive) och 1.0 (uteslutande):

```Kotlin
val randomDouble = Random.nextDouble()  // Ger ett slumptal mellan 0.0 och 1.0
println(randomDouble)
```

## Djupdykning
Att generera slumpmässiga siffror är en gammal konst i datorprogrammering. Gamla metoder inbegrep komplexa algoritmer och externa datakällor. Historiskt sett finns det dock några problem. Vanliga pseudoslumpgeneratorer skapar 'slumpmässiga' tal som faktiskt kan förutses genom att känna till tidigare tal. För att åtgärda det, tar Kotlin's `Random` funktionalitet från moderna kryptografiskt säkra pseudoslumpmässiga nummergeneratorer (CSPRNG).

Alternativt kan du använda `ThreadLocalRandom` om du har flera trådar som genererar slumpmässiga tal.

## Se Även
- [Kotlin dokumentation om Random](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.random/-random/index.html)
- [Wikipedia om pseudoslumpmässiga nummergeneratorer](https://sv.wikipedia.org/wiki/Pseudoslumptal)
- [En bra diskussion på StackOverflow om `Random` vs `ThreadLocalRandom` i Java](https://stackoverflow.com/questions/363681/how-do-i-generate-random-integers-within-a-specific-range-in-java) 

Ingen TL;DR i detta inlägg, okej? Bra programmering!