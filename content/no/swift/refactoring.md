---
title:                "Refaktorering"
date:                  2024-01-26T03:37:17.071093-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/refactoring.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Refaktorisering er prosessen med å restrukturere eksisterende dataprogramkode uten å endre dens eksterne oppførsel. Programmerere gjør dette for å rydde opp i kodebasen, forbedre lesbarheten, vedlikeholdbarheten og bane vei for fremtidige funksjoner med minimal teknisk gjeld.

## Hvordan:
La oss starte med et grunnleggende Swift-eksempel hvor vi har noe repetitiv kode:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Fornavn: \(firstName)")
    print("Etternavn: \(lastName)")
    print("Alder: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Jobbtittel: \(title)")
    print("Selskap: \(company)")
}
```

Å refaktorisere dette ville inkludere å lage en `User` struktur for å innkapsle brukerattributter og legge til en metode for å skrive ut detaljer:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Fornavn: \(firstName)")
        print("Etternavn: \(lastName)")
        print("Alder: \(age)")
        print("Jobbtittel: \(jobTitle)")
        print("Selskap: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Programvareutvikler", company: "Tech Solutions")
user.printDetails()
```

### Eksempel på utskrift:
```
Fornavn: John
Etternavn: Doe
Alder: 30
Jobbtittel: Programvareutvikler
Selskap: Tech Solutions
```

## Dypdykk
Refaktorisering har røtter som strekker seg tilbake til programvareteknikkens tidlige dager, men begrepet ble popularisert på slutten av 1990-tallet, spesielt gjennom Martin Fowlers banebrytende bok "Refactoring: Improving the Design of Existing Code". Boken la frem prinsippet om at kode kontinuerlig skal ryddes opp i små skritt i stedet for å vente på en separat fase.

Alternativer til manuell refaktorisering inkluderer automatiserte verktøy og IDE-er (Integrerte Utviklingsmiljøer) som kan bidra til å oppdage duplisert kode, foreslå forenklinger, og automatisk generere deler av koden. Xcode, for Swift-utvikling, tilbyr ulike refaktoriseringsverktøy, som navneendring og funksjonsuttrekksfunksjonalitet, som kan redusere potensialet for menneskelige feil i prosessen.

Når du implementerer refaktorisering, er det viktig å ha et solid testsuite på plass. Tester fungerer som et sikkerhetsnett, og sikrer at endringene du gjør ikke introduserer feil. Dette er viktig siden hovedmålet med refaktorisering er å endre den interne strukturen uten å påvirke den eksterne oppførselen.

## Se også
- ["Refactoring: Improving the Design of Existing Code" av Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Swift-dokumentasjon av Apple](https://swift.org/documentation/)
- [Bruke Xcodes Refaktoriseringsverktøy](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray Wenderlichs Swift-stilguide](https://github.com/raywenderlich/swift-style-guide)
