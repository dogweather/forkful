---
title:                "Swift: Konvertere en streng til små bokstaver"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hvorfor 

Det er mange forskjellige programmeringsspråk der ute, og hver av dem har sine egne funksjoner og fordeler. I Swift, et populært språk for mobilutvikling, er en av de nyttige funksjonene konvertering av strenger til små bokstaver. Dette kan være nyttig for å gjøre søk og sammenligninger i enklere og mer nøyaktige. I denne blogginnlegget skal vi se på hvorfor og hvordan du kan konvertere strenger til små bokstaver i Swift. 

## Hvordan 

For å konvertere en streng til små bokstaver i Swift, kan du bruke den innebygde funksjonen `lowercased()`. Her er et eksempel på hvordan du kan bruke denne funksjonen i koden din: 
```swift 
let navn = "JOHN" 
print(navn.lowercased()) 
```

Denne koden vil skrive ut "john", med alle bokstaver i små bokstaver. Du kan også bruke variabler eller konstanter i stedet for en fast streng, for eksempel `lowercased(navn)` for å konvertere variabelen `navn` til små bokstaver. 

En annen nyttig funksjon som er tilgjengelig, er `localizedLowercase`, som konverterer strengen basert på landets preferanse. For eksempel ville en streng i engelsk oversettes til små bokstaver ved hjelp av den engelske stavekontrollen, mens en streng i tysk ville bli konvertert basert på tysk stavekontroll. 

## Deep Dive 

Bak kulissene er konverteringen til små bokstaver basert på Unicode-standardene. Dette betyr at det også er mulig å konvertere flerspråklige strenger til små bokstaver uten problemer. En annen interessant ting å merke seg er at funksjonen `lowercased()` bare konverterer bokstaver, ikke tall eller spesialtegn. 

En ting å være oppmerksom på når du konverterer strenger til små bokstaver i Swift er at det er viktig å bruke riktig type. For eksempel, hvis du bruker `lowercased()` på en streng av typen Int, vil du få en feilmelding. Derfor er det viktig å være nøye med typene når du bruker denne funksjonen. 

## Se Også 

Håper denne bloggposten har hjulpet deg med å forstå hvordan du kan konvertere strenger til små bokstaver i Swift. For mer informasjon om sprogfunksjoner og hvordan du kan bruke dem, kan du sjekke ut disse ressursene: 

- [Swift Documentation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html) 
- [Apples offisielle YouTube-kanal for Swift-programmering](https://www.youtube.com/channel/UCoOQCXON79URL1_EqsS2-HA) 
- [Enkel guide til Swift-programmering](https://www.udemy.com/course/swift-course/)