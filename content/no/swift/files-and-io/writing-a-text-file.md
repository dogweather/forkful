---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:44.260035-07:00
description: "\xC5 skrive en tekstfil i Swift gj\xF8r at du kan lagre strengdata permanent\
  \ p\xE5 filsystemet, noe som er avgj\xF8rende for oppgaver som \xE5 lagre\u2026"
lastmod: '2024-03-13T22:44:41.160210-06:00'
model: gpt-4-0125-preview
summary: "\xC5 skrive en tekstfil i Swift gj\xF8r at du kan lagre strengdata permanent\
  \ p\xE5 filsystemet, noe som er avgj\xF8rende for oppgaver som \xE5 lagre\u2026"
title: Skrive en tekstfil
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å skrive en tekstfil i Swift gjør at du kan lagre strengdata permanent på filsystemet, noe som er avgjørende for oppgaver som å lagre konfigurasjonsinnstillinger, brukerdata eller logger. Programmerere gjør ofte dette for å opprettholde data mellom appoppstart, dele data mellom forskjellige deler av en applikasjon, eller eksportere data for bruk av andre programmer.

## Hvordan:

### Ved bruk av Swift Standardbibliotek

Swifts standardbibliotek inneholder alle verktøyene som trengs for å skrive tekstfiler. Her er en grunnleggende tilnærming:

```swift
import Foundation

let innhold = "Hallo, Wired-lesere! Å lære Swift er gøy."
let filsti = NSSearchPathForDirectoriesInDomains(.documentDirectory, .userDomainMask, true)[0] as String
let filnavn = "\(filsti)/eksempel.txt"

do {
    try innhold.write(toFile: filnavn, atomically: false, encoding: String.Encoding.utf8)
    print("Fil skrevet vellykket")
} catch let error as NSError {
    print("Mislyktes å skrive til URL: \(filnavn), Feil: " + error.localizedDescription)
}
```

Denne koden skriver en streng til en fil kalt `eksempel.txt` i dokumentmappen. Den håndterer potensielle feil ved å bruke Swifts do-try-catch feilhåndtering.

### Bruke FileManager for mer kontroll

For mer kontroll over filattributter eller for å sjekke om filen allerede eksisterer, kan `FileManager` brukes:

```swift
import Foundation

let fileManager = FileManager.default
let kataloger = fileManager.urls(for: .documentDirectory, in: .userDomainMask)
if let dokumentkatalog = kataloger.first {
    let filURL = dokumentkatalog.appendingPathComponent("eksempel.txt")
    let innhold = "Å utforske Swift for filhåndtering er opplysende."

    if fileManager.fileExists(atPath: filURL.path) {
        print("Filen eksisterer allerede")
    } else {
        do {
            try innhold.write(to: filURL, atomically: true, encoding: .utf8)
            print("Fil opprettet og skrevet vellykket")
        } catch {
            print("Feil ved skriving av fil: \(error)")
        }
    }
}
```

### Bruke Tredjepartsbiblioteker

Ett populært tredjepartsbibliotek for filsystemoperasjoner i Swift er `Files` av John Sundell:

Først, legg Files til i prosjektet ditt, vanligvis via Swift Package Manager.

```swift
// swift-tools-version:5.3
import PackageDescription

let pakke = Package(
    name: "DittPakkenavn",
    dependencies: [
        .package(url: "https://github.com/JohnSundell/Files", from: "4.0.0"),
    ],
    targets: [
        .target(
            name: "DittMålnavn",
            dependencies: ["Files"]),
    ]
)
```

Deretter, bruk det til å skrive til en fil:

```swift
import Files

do {
    let fil = try File(path: "/sti/til/din/katalog/eksempel.txt")
    try fil.write(string: "Swift og Files-biblioteket utgjør en kraftig kombinasjon.")
    print("Fil skrevet vellykket ved bruk av Files bibliotek.")
} catch {
    print("En feil oppstod: \(error)")
}
```

Med `Files`-biblioteket blir håndtering av filer mer grei, slik at du kan fokusere på forretningslogikken i applikasjonen din i stedet for den nitty-gritty av filhåndtering.
