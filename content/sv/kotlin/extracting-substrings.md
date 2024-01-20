---
title:                "Extrahera delsträngar"
html_title:           "Arduino: Extrahera delsträngar"
simple_title:         "Extrahera delsträngar"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att extrahera delsträngar innebär att ta en specifik del av en sträng baserat på position. Programmerare gör detta ofta för att bearbeta data och filtrera relevant information.

## Hur man gör

För att extrahera en delsträng i Kotlin, kan du använda `substring` funktionen. Här är några exempel:

```Kotlin
val text = "Hej, världen!"
println(text.substring(0,3))  // Utskrift: "Hej"

val namn = "Nils Svensson"
println(namn.substring(5))  // Utskrift: "Svensson"
```

I det första exemplet tar vi ut de första tre tecknen. I det andra exemplet tar vi bort de första fem tecknen och tar resten av strängen.

## Djupdykning

`substring` funktionen är en del av Strings API i Kotlin och har funnits sedan Kotlin 1.0. Dess implementation är baserad på Java's `substring`, men med mer lättanvända gränsvärden. 

En alternativ metod för att extrahera delsträngar är att använda `split` funktionen för att dela upp originalet i lösa delar. 

Implementationen av `substring` skapar en ny sträng istället för att visa till originalsträngen. Detta kan ha prestanda konsekvenser för mycket stora strängar.

## Se också

För mer information om att arbeta med strängar i Kotlin, se följande resurser:

- Officiell Kotlin dokumentation på att arbeta med strängar: [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)