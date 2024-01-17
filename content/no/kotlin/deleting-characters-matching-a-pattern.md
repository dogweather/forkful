---
title:                "Sletting av tegn som matcher et mønster"
html_title:           "Kotlin: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er en vanlig praksis blant programmører. Dette innebærer å fjerne bestemte tegn fra en streng basert på et spesifikt mønster som er definert. Dette kan være nyttig for å renske opp i uønsket data eller for å endre formatering på en mer effektiv måte.

## Hvordan:
Du kan enkelt slette tegn som matcher et mønster ved hjelp av Kotlin's ```replace()``` funksjon. For eksempel, hvis vi har følgende streng:
```
val string = "Jeg liker å spise pizza"
```
Hvis vi ønsker å slette alle bokstavene "i" i strengen, kan vi bruke følgende kode:
```
val string2 = string.replace("i", "")
```
Dette vil resultere i den oppdaterte strengen:
```
Jeg lkr å spse pzza
```

## Dypdykk:
Sletting av tegn som matcher et mønster har vært en vanlig metode i programmering i årevis. Det er en enkel og effektiv måte å manipulere data på. Alternativt kan man også bruke regular expressions for å slette tegn som matcher et mer komplekst mønster.

## Se også:
- [Kotlin sitt offisielle nettsted] (https://kotlinlang.org/)
- [Kotlin string manipulation] (https://www.callicoder.com/kotlin-string-manipulation/)