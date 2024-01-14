---
title:    "Swift: Å bruke regulære uttrykk"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

I dagens moderne verden, er programmering blitt en viktig ferdighet å ha. Uansett om du jobber som profesjonell programvareutvikler, eller bare ønsker å automatisere repetitive oppgaver, er Swift-programmering nyttig å lære. En spesifikk del av programmering verktøykassen som er viktig å være kjent med, er bruken av regulære uttrykk.

## Hvordan

For å bruke regular expressions i Swift, må du inkludere "Foundation" biblioteket og deretter importere "RegularExpression" modulen. Deretter kan du bruke `NSRegularExpression` klassen til å lage ditt regex-objekt. Her er et eksempel:

```Swift
let regex = try! NSRegularExpression(pattern: "([a-z]+)://")
let string = "https://www.example.com"
let matches = regex.matches(in: string, range: NSRange(string.startIndex..., in: string))
print(matches[0].range) // output: {0, 8}
```

I dette eksemplet, definerer vi et regex-objekt som matcher mønsteret "[a-z]+://", som er en vanlig måte å identifisere URL-er på. Deretter søker vi etter en match i en gitt streng og viser det første treffet sin posisjon og lengde.

## Dypdykk

Regular expressions er nyttig for å finne og manipulere tekststrenger basert på et bestemt mønster. De kan brukes til å validering av brukerinput, søking i store datasett, og mye mer. I Swift, er de spesielt nyttige for å arbeide med tokenisering og parsing av tekst.

En viktig del av bruk av regulære uttrykk er å forstå de forskjellige metakarakterene som kan brukes til å definere mønstre. For eksempel betyr `+` at den foregående elementet kan gjenta 1 eller flere ganger, mens `*` betyr at elementet kan gjenta 0 eller flere ganger. Det er også spesielle metakarakterer for å definere starten og slutt av en streng, samt hvilke tegn som er tillatt.

Det kan ta litt tid å lære og beherske bruken av regulære uttrykk, men når du først har lært det, vil du finne dem utrolig nyttige i mange forskjellige programmeringssituasjoner.

## Se også

- [Swift's Regular Expression Documentation](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex Tutorial for Beginners – Learn Regular Expressions](https://www.youtube.com/watch?v=rhzKDrUiJVk)
- [Regex101 - Online Regex Tester and Debugger](https://regex101.com/)