---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:06:10.535382-07:00
description: 'Hoe te: Laten we beginnen met een basisvoorbeeld in Swift waar we wat
  herhalende code hebben.'
lastmod: '2024-03-13T22:44:51.165979-06:00'
model: gpt-4-0125-preview
summary: Laten we beginnen met een basisvoorbeeld in Swift waar we wat herhalende
  code hebben.
title: Refactoring
weight: 19
---

## Hoe te:
Laten we beginnen met een basisvoorbeeld in Swift waar we wat herhalende code hebben:

```Swift
func printUserDetails(firstName: String, lastName: String, age: Int) {
    print("Voornaam: \(firstName)")
    print("Achternaam: \(lastName)")
    print("Leeftijd: \(age)")
}

func printUserJob(title: String, company: String) {
    print("Functietitel: \(title)")
    print("Bedrijf: \(company)")
}
```

Het refactoren hiervan zou het creëren van een `User` struct omvatten om gebruikersattributen te encapsuleren en een methode toe te voegen om details af te drukken:

```Swift
struct User {
    let firstName: String
    let lastName: String
    let age: Int
    let jobTitle: String
    let company: String

    func printDetails() {
        print("Voornaam: \(firstName)")
        print("Achternaam: \(lastName)")
        print("Leeftijd: \(age)")
        print("Functietitel: \(jobTitle)")
        print("Bedrijf: \(company)")
    }
}

let user = User(firstName: "John", lastName: "Doe", age: 30, jobTitle: "Softwareontwikkelaar", company: "Tech Solutions")
user.printDetails()
```

### Voorbeelduitvoer:
```
Voornaam: John
Achternaam: Doe
Leeftijd: 30
Functietitel: Softwareontwikkelaar
Bedrijf: Tech Solutions
```

## Diepere Duik
Refactoring heeft wortels die teruggaan tot de vroege dagen van software-engineering, maar de term werd gepopulariseerd in de late jaren 90, met name door het baanbrekende boek van Martin Fowler "Refactoring: Improving the Design of Existing Code". Het boek legde het principe uit dat code continu opgeruimd moet worden in kleine stappen in plaats van te wachten op een aparte fase.

Alternatieven voor handmatige refactoring omvatten geautomatiseerde tools en IDE's (Integrated Development Environments) die kunnen helpen bij het detecteren van dubbele code, het voorstellen van vereenvoudigingen en het automatisch genereren van delen van de code. Xcode, voor Swift-ontwikkeling, biedt verschillende refactoringtools, zoals functies voor hernoemen en methode-extractie, die de kans op menselijke fouten in het proces kunnen verminderen.

Bij het implementeren van refactoring is het belangrijk om een solide testsuite op zijn plaats te hebben. Tests fungeren als een vangnet en zorgen ervoor dat de wijzigingen die je aanbrengt geen bugs introduceren. Dit is essentieel, aangezien het belangrijkste doel van refactoring is om de interne structuur te wijzigen zonder het externe gedrag te beïnvloeden.

## Zie Ook
- ["Refactoring: Improving the Design of Existing Code" door Martin Fowler](http://martinfowler.com/books/refactoring.html)
- [Swift-documentatie door Apple](https://swift.org/documentation/)
- [Gebruik van Xcode Refactoring Tools](https://help.apple.com/xcode/mac/current/#/dev91fe7130a)
- [Swift-stijlgids van Ray Wenderlich](https://github.com/raywenderlich/swift-style-guide)
