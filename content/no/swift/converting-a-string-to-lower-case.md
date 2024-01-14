---
title:    "Swift: Konvertere en streng til små bokstaver"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være nødvendig å konvertere en streng til små bokstaver når du jobber med tekstbehandling og ønsker å behandle alle bokstavene på samme måte uten å ta hensyn til store og små bokstaver.

## Hvordan
Konvertering av en streng til små bokstaver er en enkel og nyttig funksjon som kan gjøres ved å bruke `.lowercased()` metoden. Se et eksempel i kodesnutten nedenfor:

```Swift
let navn = "Marie"
print(navn.lowercased())
```

Dette vil gi følgende utdata:

```Swift
marie
```

## Dypdykk
Når en streng blir konvertert til små bokstaver, utføres denne prosessen bruk av Unicode standarden. Dette betyr at det også vil konvertere eventuelle spesielle tegn til den tilsvarende små bokstaven. For eksempel vil "Å" bli konvertert til "å" og "Ø" til "ø".

Det er også verdt å merke seg at `.lowercased()` metoden vil ignorere eventuelle tall eller symboler, og bare konvertere alfabetiske bokstaver til små bokstaver. Dette gjør at denne metoden kan brukes på en rekke ulike typer strenger.

## Se også
- [Offisiell Swift dokumentasjon for `.lowercased()` metode](https://developer.apple.com/documentation/swift/string/2995525-lowercased)
- [Konvertering av en Streng til Store Bokstaver i Swift] (https://www.blogty.com/swift-articles/convert-to-uppercase-in-swift/)