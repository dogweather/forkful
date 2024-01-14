---
title:                "Swift: Å opprette en midlertidig fil"
programming_language: "Swift"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lage midlertidige filer kan være et nyttig verktøy i Swift-programmering. Disse filene lar deg midlertidig lagre data eller informasjon mens du kjører koden din, og sletter deretter filen når den ikke lenger er nødvendig. Dette kan være spesielt nyttig når du arbeider med store mengder data eller trenger å lagre informasjon som bare er nødvendig for en kort periode.

## Hvordan

For å opprette en midlertidig fil i Swift, kan du bruke "FileManager" -klassen og dens "createFile" -metode. Her er et eksempel på hvordan du oppretter en midlertidig tekstfil og legger til noe tekst i den:

```Swift
let tempFile = FileManager.default.temporaryDirectory.appendingPathComponent("mittMidlertidigeProsjekt.txt")

do {
    try "Dette er mitt midlertidige prosjekt!".write(to: tempFile, atomically: true, encoding: .utf8)
    print("Midlertidig fil opprettet")
    print("Innholdet i filen er:")
    print(try String(contentsOf: tempFile))
} catch {
    print("Kunne ikke opprette midlertidig fil: \(error)")
}
```

Dette eksemplet bruker "tempDirectory" -egenskapen til "FileManager" -klassen, som vil opprette filen i et midlertidig katalog på datamaskinen din. Deretter bruker den "write" -metoden for å legge til teksten i filen, og deretter skriver ut filen innholdet ved hjelp av "contentsOf" -metoden.

## Dypdykk

Når du oppretter en midlertidig fil, bør du også sørge for å slette den når den ikke lenger er nødvendig. Dette kan gjøres ved hjelp av "removeItem" -metoden til "FileManager" -klassen. Det er også viktig å merke seg at disse midlertidige filene bare er ment for midlertidig lagring og bør ikke brukes som en permanent lagringsløsning.

En annen nyttig funksjon når du jobber med midlertidige filer er "URLSession" -klassen. Denne klassen lar deg laste ned data og lagre den i en midlertidig fil før du bruker den videre i koden din.

## Se Også

- [FileManager dokumentasjon](https://developer.apple.com/documentation/foundation/filemanager)
- [URLSession dokumentasjon](https://developer.apple.com/documentation/foundation/urlsession)
- [Swift koden for dette eksemplet](https://github.com/apple/swift/blob/master/test/stdlib/FileManager.swift)