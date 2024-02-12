---
title:                "Sette stor bokstav i en streng"
date:                  2024-02-03T19:06:29.518801-07:00
model:                 gpt-4-0125-preview
simple_title:         "Sette stor bokstav i en streng"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/capitalizing-a-string.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Hva og hvorfor?

Å sette stor forbokstav i en streng i Swift endrer den gitte strengen slik at dens første tegn er med stor bokstav, og de gjenværende tegnene er med små bokstaver. Programmerere gjør dette av grunner som å formatere navn eller setninger i henhold til grammatiske regler eller standarder for brukergrensesnitt.

## Hvordan:

Swifts `String`-strukturer kommer med et par innebygde metoder for å manipulere små og store bokstaver i strenger. Her er noen tilnærminger for å sette stor bokstav i strenger i Swift, inkludert bruk av standardmetoder og tredjepartsbiblioteker om nødvendig.

### Bruk av innebygde metoder

For å sette stor forbokstav i første bokstav av en streng og gjøre resten små:

```swift
let myString = "hello, world"
let capitalizedString = myString.prefix(1).uppercased() + myString.dropFirst().lowercased()
print(capitalizedString) // Resultat: "Hello, world"
```

For å sette stor bokstav i første bokstav av hvert ord i en setning, kan du bruke `capitalized`-egenskapen:

```swift
let sentence = "hello, world"
let capitalizedSentence = sentence.capitalized
print(capitalizedSentence) // Resultat: "Hello, World"
```

### Bruk av et tredjepartsbibliotek

Selv om Swifts standardbibliotek er ganske omfattende, kan noen spesifikke formater for stor bokstav kreve mer komplekse operasjoner eller kan forenkles ved bruk av tredjepartsbiblioteker. Ett av de populære for strengmanipulering er SwiftRichString. (Merk: Sørg alltid for å inkludere tredjepartsbiblioteker gjennom Swift Package Manager, CocoaPods, eller Carthage, og importer dem i filen din.)

Først må du legge til `SwiftRichString` i prosjektet ditt. Når installert, kan du bruke det til å utføre ulike strengoperasjoner, inkludert spesifikke behov for stor bokstav. Men, per nå, dekker Swifts innebygde metoder tilstrekkelig de fleste behov for stor bokstav uten å trenge eksterne biblioteker bare for å sette stor bokstav i strenger.

Henvis alltid til den nyeste dokumentasjonen av biblioteket for eventuelle oppdateringer eller endringer i metoder.
