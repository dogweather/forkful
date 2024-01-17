---
title:                "Søke og bytte ut tekst"
html_title:           "Swift: Søke og bytte ut tekst"
simple_title:         "Søke og bytte ut tekst"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstattelse av tekst er en vanlig oppgave for programutviklere. Det er når du søker gjennom en tekst og erstatter ett eller flere deler av teksten med noe annet. Programmerere gjør dette for å effektivt endre og oppdatere store mengder kode.

## Hvordan:
For å søke og erstatte tekst i Swift, kan du bruke `replacingOccurrences(of:with:)` metoden. Denne metoden tar inn to parametere - den første er teksten du vil søke etter, og den andre er teksten du vil erstatte den med. Her er et eksempel:

```Swift
var text = "Hei verden!"
text = text.replacingOccurrences(of: "Hei", with: "Hallo")
// Output: "Hallo verden!"
```

Du kan også bruke `range(of:)` metoden for å finne posisjonen til teksten du vil bytte ut. Dette er nyttig når du vil erstatte bare en del av teksten. Her er et eksempel:

```Swift
var text = "Dette er en tekst"
if let range = text.range(of: "tekst") {
    text.replaceSubrange(range, with: "setning")
}
// Output: "Dette er en setning"
```

## Dypdykk:
Søking og erstattelse av tekst har eksistert siden begynnelsen av programmering. Før i tiden var det gjort manuelt, men nå er det innebygd i mange programmeringsspråk som en enkel metode. En alternativ tilnærming er å bruke et regulært uttrykk, noe som gir mer fleksibilitet og kraft i søkeprosessen. Implementasjonsdetaljer kan variere mellom språk og biblioteker, men konseptet er det samme.

## Se også:
- [Apple Documentation on replacingOccurrences](https://developer.apple.com/documentation/swift/string/1786175-replacingoccurrences)
- [Regular Expressions in Swift](https://medium.com/@abhimuralidharan/regular-expression-in-swift-7c40cf2289c1)