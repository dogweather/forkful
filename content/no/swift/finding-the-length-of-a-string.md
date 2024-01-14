---
title:    "Swift: Finn lengden på en streng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Har du noen gang lurt på hvor mange tegn som er i en tekststreng? Det kan være nyttig å vite lengden på en streng når du jobber med manipulering av tekst i Swift. I denne bloggposten vil jeg vise deg hvordan du enkelt kan finne lengden på en streng ved hjelp av enkle kodesnutter.

## Hvordan
For å finne lengden på en streng i Swift kan du bruke metoden `count` som er tilgjengelig for alle strenger. La oss si at du har en streng som heter `navn` og du ønsker å finne lengden på den. Her er en kodestump som viser hvordan du kan gjøre dette:

```Swift
let navn = "Johan"
print(navn.count)
```

Output vil være `5`, siden strengen "Johan" består av fem tegn. Hvis du ønsker å finne lengden på en tom streng, vil metoden `count` returnere `0`.

Det er også verdt å merke seg at metoden `count` ikke tar hensyn til om bokstaver er store eller små, den bare teller antall tegn i strengen.

## Deep Dive
For å forstå hvordan `count`-metoden fungerer, er det viktig å ha en grunnleggende forståelse av Unicode og UTF-8. Unicode er et tegnsett som brukes for å representere tekst i forskjellige språk og skriftsystemer. UTF-8 er en måte å lagre Unicode-tegn på i datamaskinen. Når en streng blir representert i UTF-8, vil hver tegn ha en fast størrelse på 8 bits eller en byte.

`count`-metoden fungerer ved å telle antall bytes i strengen. Siden hver bokstav i UTF-8 er representert med en byte, vil metoden returnere antall tegn i strengen.

## Se også
- [Metoder for strenger i Swift](https://developer.apple.com/documentation/swift/string_methods)
- [Enkel håndbok i Unicode](https://home.unicode.org/basic-info/)

Lykke til med å finne lengden på strenger i Swift!