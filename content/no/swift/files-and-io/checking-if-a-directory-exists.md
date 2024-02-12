---
title:                "Sjekker om en mappe eksisterer"
aliases: - /no/swift/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:41.318032-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sjekker om en mappe eksisterer"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva & hvorfor?
Å sjekke om en katalog eksisterer i filsystemet er essensielt for å håndtere filstrukturer fra dine Swift-applikasjoner. Denne oppgaven gjør det mulig for utviklere å verifisere tilstedeværelsen av kataloger før de forsøker å lese fra eller skrive til dem, og dermed unngå mulige kjøretidsfeil.

## Hvordan:

Swifts Foundation-rammeverk tilbyr `FileManager`-klassen, som har metoder for å håndtere filsystemet. Du kan bruke `FileManager` for å sjekke om en katalog eksisterer. Her er et kodeutdrag om hvordan du gjør dette:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/sti/til/din/katalog"

if fileManager.fileExists(atPath: path, isDirectory: nil) {
    print("Katalogen eksisterer")
} else {
    print("Katalogen eksisterer ikke")
}
```

Men, dette sjekker for både filer og kataloger. Hvis du spesifikt ønsker å verifisere at en katalog eksisterer, må du sende en peker til en Boolesk verdi i `isDirectory`:

```swift
import Foundation

let fileManager = FileManager.default
let path = "/sti/til/din/katalog"
var isDirectory: ObjCBool = false

if fileManager.fileExists(atPath: path, isDirectory: &isDirectory), isDirectory.boolValue {
    print("Katalogen eksisterer")
} else {
    print("Katalogen eksisterer ikke")
}
```

### Bruke et tredjepartsbibliotek

Per nå krever vanligvis ikke sjekking for eksistensen av en katalog i Swift tredjepartsbiblioteker på grunn av robustheten til `FileManager`-klassen. Men, for mer kompleks filmanipulasjon og sjekking, tilbyr biblioteker som **Files** av John Sundell en mer Swift-vennlig API.

Slik kan du bruke det:

Først, legg til Files i prosjektet ditt via Swift Package Manager.

Deretter kan du sjekke for eksistensen av en katalog slik:

```swift
import Files

do {
    _ = try Folder(path: "/sti/til/din/katalog")
    print("Katalogen eksisterer")
} catch {
    print("Katalogen eksisterer ikke")
}
```

Merk: Siden tredjepartsbiblioteker kan endres, referer alltid til den nyeste dokumentasjonen for bruk og beste praksiser.
