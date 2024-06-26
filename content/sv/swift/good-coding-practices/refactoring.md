---
date: 2024-01-26 03:36:59.770117-07:00
description: "Hur man g\xF6r: L\xE5t oss b\xF6rja med ett grundl\xE4ggande Swift-exempel\
  \ d\xE4r vi har lite repetitiv kod."
lastmod: '2024-03-13T22:44:38.259321-06:00'
model: gpt-4-0125-preview
summary: "L\xE5t oss b\xF6rja med ett grundl\xE4ggande Swift-exempel d\xE4r vi har\
  \ lite repetitiv kod."
title: Refaktorisering
weight: 19
---

## Hur man gör:
Låt oss börja med ett grundläggande Swift-exempel där vi har lite repetitiv kod:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Förnamn: \(firstName)")
    print("Efternamn: \(lastName)")
    print("Ålder: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Jobbtitel: \(title)")
    print("Företag: \(company)")
}
```

Att refaktorisera detta skulle inkludera att skapa en `User`-struktur för att inkapsla användarattribut och lägga till en metod för att skriva ut detaljer:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Förnamn: \(firstName)")
        print("Efternamn: \(lastName)")
        print("Ålder: \(age)")
        print("Jobbtitel: \(jobTitle)")
        print("Företag: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Mjukvaruutvecklare", company: "Tech Solutions")
user.printDetails()
```

### Exempelutskrift:
```
Förnamn: John
Efternamn: Doe
Ålder: 30
Jobbtitel: Mjukvaruutvecklare
Företag: Tech Solutions
```

## Fördjupning
Refaktorisering har rötter som går tillbaka till de tidiga dagarna av mjukvaruutveckling, men termen populariserades i slutet av 1990-talet, särskilt genom Martin Fowlers banbrytande bok "Refaktorisering: Att förbättra designen av befintlig kod". Boken lade fram principen att koden kontinuerligt bör städas upp i små steg istället för att vänta på en separat fas.

Alternativ till manuell refaktorisering inkluderar automatiserade verktyg och IDE:s (Integrated Development Environments) som kan hjälpa till att upptäcka duplicerad kod, föreslå förenklingar och auto-generera delar av koden. Xcode, för Swift-utveckling, erbjuder olika refaktoriseringsverktyg, såsom omdöpning och extraktionsmetodfunktionalitet, som kan minska potentialen för mänskliga fel i processen.

När man implementerar refaktorisering är det viktigt att ha en solid testsvit på plats. Tester fungerar som ett säkerhetsnät och säkerställer att de ändringar du gör inte introducerar buggar. Detta är avgörande eftersom huvudmålet med refaktorisering är att ändra den interna strukturen utan att påverka det externa beteendet.

## Se också
- ["Refaktorisering: Att förbättra designen av befintlig kod" av Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Swift-dokumentation av Apple](https://swift.org/documentation/)
- [Användning av Xcodes refaktoriseringsverktyg](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Ray Wenderlichs Swift-stilguide](https://github.com/raywenderlich/swift-style-guide)
