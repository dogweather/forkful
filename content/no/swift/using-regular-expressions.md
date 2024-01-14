---
title:                "Swift: Å bruke regulære uttrykk"
simple_title:         "Å bruke regulære uttrykk"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvis du har programmert en stund, har du kanskje hørt om noe som kalles "regular expressions". Men hva er egentlig dette, og hvorfor bør du bry deg om det? Regular expressions er en måte å søke etter og manipulere tekst på en effektiv måte. De kan være spesielt nyttige når du arbeider med store datasett eller utfører komplekse tekstbehandlingsoppgaver.

## Hvordan

For å bruke regular expressions i Swift, må du først importere Foundation frameworket ved å legge til ```import Foundation``` øverst i filen din. Deretter kan du lage en ny instans av NSRegularExpression klassen og gi den et mønster (pattern) å søke etter. Her er et enkelt eksempel som viser hvordan du kan finne alle strenger som inneholder et bestemt ord:

```
let regex = try! NSRegularExpression(pattern: "apple") // lager en regulær uttrykksinstans med mønsteret "apple"
let testString = "Jeg liker å spise eple og drikke eplejuice." 
let range = NSRange(location: 0, length: testString.utf16.count) // finner området av strengen som skal søkes gjennom
let matches = regex.matches(in: testString, range: range) // utfører søk og lagrer resultatene i en array

for match in matches {
    let matchRange = match.range // finner posisjonen til hvert treff
    let matchString = (testString as NSString).substring(with: matchRange) // henter ut teksten som matcher mønsteret
    print(matchString) // skriver ut hver matchende streng
}
```

Output'en for dette eksemplet blir:

```
apple
eple
eple
```

## Deep Dive

Regular expressions kan virke komplisert ved første øyekast, men de kan være en kraftig verktøy å ha i verktøykassen din som programmerer. Her er noen ekstra tips og triks for å hjelpe deg med å få mest mulig ut av dem:

- Kom i gang ved å lære de grunnleggende mønstrene som brukes i regular expressions, som for eksempel "wildcards" og kvantifikatorer.
- Bruk en online regex-tester, som Regexr eller RegEx101, for å eksperimentere med forskjellige mønstre og se hvordan de påvirker søkene dine.
- Ta deg tid til å lære om forskjellige verktøy og programmeringspråk som støtter regular expressions, da syntaks og funksjonalitet kan variere.
- Ikke vær redd for å bruke regular expressions i dine egne programmeringsprosjekter! Med litt øvelse kan de spare deg for mye tid og frustrasjon.

## Se også

- [Swift Dokumentasjon: Regular Expressions](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [10 tips for å mestre regulære uttrykk](https://www.infoworld.com/article/3197225/app-development/confused-by-regular-expressions-ease-the-pain-with-these-examples.html)
- [RegExr: Online regulær uttrykk tester og visuelt design verktøy](https://regexr.com/)