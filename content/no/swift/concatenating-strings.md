---
title:                "Swift: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor
Noen ganger er det nødvendig å sette sammen flere strenger til én lang streng i Swift-programmering. Dette kan være nyttig for å lage dynamiske strenger som inneholder variabel data, som for eksempel en brukers navn eller alder, ved siden av fast tekst. Slik kan du enkelt tilpasse meldinger og informasjon i appen din.

## Hvordan
Det er flere måter å konkatenerer strenger i Swift på, men den enkleste metoden er å bruke operatøren `+` eller kommandoen `append()`. La oss se på et eksempel:

```Swift
let navn = "Ingrid"
let alder = 29
print("Hei, mitt navn er " + navn + " og jeg er " + String(alder) + " år gammel.")
```

Dette vil resultere i en utskrift av "Hei, mitt navn er Ingrid og jeg er 29 år gammel." ved hjelp av `+`-operatøren. Ved å bruke `append()`-kommandoen, kan vi skrive det samme eksemplet som:

```Swift
var personInfo = "Hei, mitt navn er "
personInfo.append(navn)
personInfo.append(" og jeg er ")
personInfo.append(String(alder))
personInfo.append(" år gammel.")
print(personInfo)
```

Dette vil også gi utskriften vår.

## Dykk dypere
I Swift er det også mulig å interpolere strenger ved hjelp av `\(variable)`. Dette betyr at i stedet for å bruke `+` eller `append()`, kan vi inkludere variabler direkte i strengene våre. La oss se på et eksempel:

```Swift
let favorittMat = "pizza"
print("Jeg elsker å spise \(favorittMat) til middag.")
```

Dette vil gi utskriften vår av "Jeg elsker å spise pizza til middag." Som du kan se, gjør dette det enklere å inkludere og formatere variabler i strengene våre.

## Se også
* [Swift String and Character documentation](https://developer.apple.com/documentation/swift/string)
* [Free Swift tutorials and resources](https://www.hackingwithswift.com)
* [Swift docs på norsk](https://swiftlang.nga.gov.au)