---
title:    "Swift: Å bruke regulære uttrykk"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor
Å bruke regulære uttrykk kan være en effektiv måte å søke og manipulere tekst på i programmering. Det kan hjelpe deg å finne spesifikke mønstre eller ord i en tekst, og dermed automatisere prosesser og spare tid.

## Slik gjør du det
Regulære uttrykk brukes ofte i Swift programmering for å søke og matche mønstre i tekst. Her er et enkelt eksempel på hvordan du kan bruke regulære uttrykk for å finne alle tall i en tekststreng:

```Swift
let input = "Hei 123 Hva skjer 456"
let regex = try! NSRegularExpression(pattern: "[0-9]+")
let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))

for match in matches {
  print(String(input[Range(match.range, in: input)!]))
}

// Output:
// 123
// 456
```

Som du kan se, bruker vi NSRegularExpression klassen fra Swift Foundation frameworket for å finne og hente ut tall fra en gitt tekststreng. Vi definerer også et mønster ved hjelp av regex syntax "[0-9]+" som betyr at det skal matches alle tall fra 0 til 9.

## Dypdykk
Regulære uttrykk kan være komplekse og har en stor mengde muligheter for å matche tekst. Det finnes også flere forskjellige syntaks og metoder for å bruke regulære uttrykk i Swift programmering. Det er derfor viktig å lese og forstå dokumentasjonen for å bruke de riktig.

Det er også viktig å tenke på ytelse når du bruker regulære uttrykk, spesielt hvis du skal søke i store tekstfiler. Det kan være lurt å teste og justere regex uttrykket for å optimalisere søketiden.

## Se også
- [Official Swift documentation for NSRegularExpression](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Regex tutorial by Ray Wenderlich](https://www.raywenderlich.com/5768490-regular-expressions-tutorial-swift-5-1)
- [Regex cheat sheet by RegexOne](https://regexone.com/cheat-sheet)