---
title:    "Swift: Å finne lengden av en streng"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

En av de grunnleggende oppgavene i programmering er å håndtere data. Det kan være alt fra tall til tekststrenger. For å kunne behandle disse dataene på en effektiv måte, er det viktig å kjenne til egenskapene og funksjonene til de ulike datatypene. I denne artikkelen vil vi se nærmere på hvordan man kan finne lengden til en tekststreng i Swift, og hvorfor dette er nyttig i programmering.

## Hvordan

For å finne lengden til en tekststreng i Swift, kan man bruke funksjonen `count` fra Swifts standardbibliotek. Denne funksjonen tar inn en tekststreng som parameter og returnerer antall tegn i strengen. La oss se på et eksempel:

```Swift
let tekst = "Hei, verden!"
let lengde = tekst.count

print(lengde) // Output: 13
```

Her har vi definert en variabel `tekst` med verdien "Hei, verden!" og brukt `count`-funksjonen for å finne lengden til tekststrengen. Denne lengden er deretter lagret i variabelen `lengde` og blir så printet ut til konsollen. Man kan også bruke `count`-funksjonen direkte i `print`-funksjonen for å unngå å bruke en ekstra variabel.

```Swift
print("Hei, verden!".count) // Output: 13
```

Det er viktig å merke seg at `count`-funksjonen teller alle tegn i en tekststreng, inkludert mellomrom og spesialtegn som for eksempel komma og utropstegn.

## Deep Dive

Gjennom å bruke `count`-funksjonen kan man også finne lengden til en tekststreng som er lagret i en array, ved å bruke indeksering. Indeksering er en måte å få tilgang til bestemte elementer i en array eller tekststreng på, ved å bruke et tall som angir posisjonen til elementet. La oss se på et eksempel:

```Swift
let tekster = ["Hei, verden!", "Hallo", "God dag"]

print(tekster[0].count) // Output: 13
print(tekster[1].count) // Output: 5
print(tekster[2].count) // Output: 7
```

Her har vi definert en array `tekster` som inneholder tre tekststrenger. Ved å bruke indeksering kan vi få tilgang til hver enkelt tekststreng og bruke `count`-funksjonen på dem. Dette kan være nyttig hvis man for eksempel vil separere en lengre tekststreng og finne lengden på en bestemt del av den.

## Se også

- [Swift `String` documentation](https://developer.apple.com/documentation/swift/string)
- [Swift `Array` documentation](https://developer.apple.com/documentation/swift/array)
- [How to Work with Strings in Swift](https://www.hackingwithswift.com/read/0/strings-www-hackingwithswift-com/how-to-work-with-strings-in-swift)