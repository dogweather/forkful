---
title:    "Swift: Skriver en tekstfil"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Hvorfor

Å skrive en tekstfil kan være nyttig av mange forskjellige grunner i et Swift-program. Det kan brukes til å lagre informasjon som trengs for å kjøre programmet, som for eksempel brukerinnstillinger. Det kan også være nyttig for å lagre data som kan brukes senere, som for eksempel en historikk av tidligere handlinger.

## Hvordan

Det er enkelt å skrive en tekstfil i Swift ved hjelp av `FileManager`-klassen. Her er et eksempel på å skrive en enkel tekstfil med navnet "tekstfil.txt" til brukerens dokumentmappe:

```Swift
let tekst = "Hei verden!"
let filsti = FileManager.default.urls(for: .documentDirectory, in: .userDomainMask).first!.appendingPathComponent("tekstfil.txt")

do {
    try tekst.write(to: filsti, atomically: true, encoding: .utf8)
    print("Tekstfilen ble skrevet!")
} catch {
    print("Det oppsto en feil ved skriving av tekstfilen.")
}
```

Dette eksempelet viser også hvordan du kan håndtere eventuelle feil som kan oppstå under skrivingen av tekstfilen.

For å lese en tekstfil, kan du bruke `String`-klassen sin `init(contentsOf:encoding:)`-metode. Her er et eksempel på å lese innholdet fra tekstfilen som vi nettopp opprettet og skrev til, og deretter skrive det ut:

```Swift
if let innhold = String(contentsOf: filsti, encoding: .utf8) {
    print(innhold) // Skriver ut "Hei verden!"
} else {
    print("Kunne ikke lese tekstfilen.")
}
```

## Deep Dive

En tekstfil kan også brukes til å lagre mer komplekse datastrukturer, som for eksempel en liste med brukernavn og passord. Dette kan gjøres ved hjelp av Swift's `Codable`-protokoll. Du kan opprette en egendefinert struktur som inneholder informasjonen du vil lagre, og implementere `Codable`-protokollen som gjør at strukturen kan enkelt både serialiseres og deserialiseres til/fra en tekstfil.

## Se også

- [Swift's FileManager](https://developer.apple.com/documentation/foundation/filemanager)
- [Swift's String](https://developer.apple.com/documentation/swift/string)
- [Swift's Codable](https://developer.apple.com/documentation/swift/codable)