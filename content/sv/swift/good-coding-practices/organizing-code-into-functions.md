---
date: 2024-01-26 01:16:09.344776-07:00
description: "Hur man g\xF6r: T\xE4nk dig en uppgift: ber\xE4kna genomsnittet av en\
  \ array. Utan funktioner skulle du klumpa ihop allt i main. Med funktioner g\xF6\
  r du s\xE5 h\xE4r."
lastmod: '2024-03-13T22:44:38.256549-06:00'
model: gpt-4-0125-preview
summary: "T\xE4nk dig en uppgift."
title: Organisera kod i funktioner
weight: 18
---

## Hur man gör:
Tänk dig en uppgift: beräkna genomsnittet av en array. Utan funktioner skulle du klumpa ihop allt i main. Med funktioner gör du så här:

```swift
func calculateAverage(of numbers: [Double]) -> Double {
    let sum = numbers.reduce(0, +)
    return numbers.isEmpty ? 0 : sum / Double(numbers.count)
}

// Användning
let scores = [92.5, 88.75, 99.0, 70.5]
let averageScore = calculateAverage(of: scores)
print("Genomsnittspoängen är \(averageScore)")
```

Exempelutskriften skulle vara: 
```
Genomsnittspoängen är 87.6875
```

## Fördjupning
Historiskt sett, när programmeringen blev mer komplex, blev funktioner en hörnsten för att hantera komplexiteten. Alternativ inkluderar inlinad kod och kopiera-klistra in kod (spaghettikod) – nu till stor del ansedd som dålig praxis. I Swift är funktioner medborgare i första klass; de kan tilldelas till variabler, skickas som argument och returneras från andra funktioner, vilket gör koden mer modulär och flexibel.

När det gäller implementeringen, designa dina funktioner för att göra en sak väl. Sikta på funktioner med ett tydligt syfte och ett namn som återspeglar det. Var uppmärksam på antalet parametrar – för många och du gör förmodligen för mycket. Felhantering? Överväg att använda funktioner som kastar fel och hantera problem på ett angenämt sätt. Kom ihåg: Swift handlar allt om läsbarhet och lätthet att underhålla.

## Se även
- [Swift Programmeringsspråksguide - Funktioner](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray Wenderlichs Swift-stilguide](https://github.com/raywenderlich/swift-style-guide)
- [Martin Fowlers Refactoring: Improving the Design of Existing Code](https://martinfowler.com/books/refactoring.html)
