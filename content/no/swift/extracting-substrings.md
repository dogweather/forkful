---
title:    "Swift: Ekstrahering av delstrenger"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å trekke ut substrings er en nyttig teknikk i Swift-programmering for å arbeide med deler av en streng. Dette kan være nyttig når du for eksempel trenger å hente ut et telefonnummer fra en lengre tekst eller når du ønsker å dele opp en adresse i ulike deler.

## Hvordan

For å trekke ut en substring, bruker du metoden `prefix` eller `suffix` og angir hvilken del av strengen du ønsker å hente ut. La oss se på et eksempel:

````Swift
let navn = "Lars Olsen"
let etternavn = navn.suffix(5)

print(etternavn)

// Output: Olsen
````

I dette eksempelet brukte vi `suffix`-metoden for å hente ut de siste 5 tegnene i strengen `navn`, som er etternavnet vårt. Du kan også bruke `prefix` på samme måte for å hente ut de første tegnene i en streng.

Det er også mulig å bruke `Range`-operatøren for å trekke ut en del av en streng. For eksempel:

````Swift
let tall = "123456789"
let tredjeTilSjette = tall[2...5]

print(tredjeTilSjette)

// Output: 3456
````

I dette tilfellet gir vi to indekser til `Range`-operatøren, som indikerer hvilke tegn som skal hentes ut.

## Deep Dive

Når du bruker `prefix` og `suffix`, må du passe på at du ikke prøver å hente ut et større antall tegn enn det som finnes i strengen. Ellers vil du få en runtime-feil. Pass også på at hvis du bruker `prefix` eller `suffix` på en Unicode-streng, kan du ende opp med uønsket oppførsel. Det er derfor viktig å alltid teste koden din grundig for å sikre at du får de resultatene du forventer.

Det er også verdt å merke seg at de fleste av metodene for å trekke ut substrings i Swift returnerer en `Substring`-type i stedet for en `String`-type. Dette betyr at du kanskje må konvertere den til en `String` før du kan bruke den videre.

## Se også

- [Offisiell Swift dokumentasjon for `prefix` og `suffix`](https://developer.apple.com/documentation/swift/substring)
- [Stack Overflow: How to extract a substring in Swift?](https://stackoverflow.com/questions/39677330/how-to-extract-a-substring-in-swift)