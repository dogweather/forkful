---
title:                "Slette tegn som matcher et mønster"
html_title:           "Swift: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å slette tegn som matcher et mønster er en vanlig oppgave innen programmering. Dette innebærer å fjerne tegn fra en tekststreng som oppfyller et spesifikt kriterium, for eksempel å fjerne alle tall fra en streng med tekst. Programmere gjør dette for å enkelt foreta endringer i teksten uten å måtte gå gjennom og fjerne tegn manuelt.

## Hvordan:
Her er et eksempel på hvordan du kan slette tall fra en tekststreng i Swift:

```Swift
let tekst = "Hello123 World456"
let kunBokstaver = tekst.replacingOccurrences(of: "[0-9]", with: "", options: .regularExpression)
print(kunBokstaver) // Output: Hello World
```

Her bruker vi `.replacingOccurrences` -metoden for å erstatte tallene som matcher mønsteret "[0-9]" med en tom streng. Dette gir oss en ny tekststreng uten tall.

## Dypdykk:
Denne funksjonen er tilgjengelig i mange programmeringsspråk, og har blitt mye brukt siden tekstbehandlingsprogrammer ble utviklet. Alternativer til å bruke mønstermatching inkluderer å bruke .filter-metoden i Swift for å filtrere ut tegnene som ikke passer med kriteriet, men dette kan føre til mer komplisert kode.

## Se også:
- [Swift String Documentation](https://developer.apple.com/documentation/swift/string)
- [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html)