---
title:    "Swift: Att få den aktuella datumet"
keywords: ["Swift"]
---

{{< edit_this_page >}}

## Varför 

Att kunna få ut den aktuella datumet är en grundläggande funktion inom programmering som är användbar för en mängd olika program. Oavsett om det är för att visa användarens födelsedag, markera ett viktigt datum eller skapa en tidsstämpel för ett dokument, kan du enkelt få tillgång till det aktuella datumet i din kod med hjälp av Swift.

## Hur man gör

För att få det aktuella datumet i Swift kan du använda klassen `Date()`. Detta kommer att ge dig en instans av det aktuella datumet och tiden. För att sedan kunna manipulera datumen eller extrahera specifik information från det, behöver du använda klassen `DateComponents`. 

```swift
// Skapar en instans av det aktuella datumet och tiden
let nu = Date()

// Skapar en instans av DateComponents för att kunna få ut specifik information från datumet
let komponenter = Calendar.current.dateComponents([.year, .month, .day], from: nu)

// Extraherar information från instansen och lagrar den i variabler
let år = komponenter.year
let månad = komponenter.month
let dag = komponenter.day

print("Aktuellt datum: \(dag)/\(månad)/\(år)") // Output: Aktuellt datum: 30/12/2019
```

## Djupdykning

Genom att använda `Date()` och `DateComponents` kan du få tillgång till många olika funktioner och metoder för att hantera datum och tid i din kod. Du kan till exempel använda `date(byAdding: <komponenter>, to: <datum>)` för att lägga till eller dra ifrån en viss tid från ett datum eller `isDateInYesterday()` för att kolla om ett visst datum är gårdagens datum. Det finns också olika format för datum och tid som du kan använda dig av genom att använda `DateFormatter()`.

## Se även

- [Apple Developer Documentation - Date()](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateComponents()](https://developer.apple.com/documentation/foundation/datecomponents)
- [Swift by Sundell - Working with dates and times in Swift](https://www.swiftbysundell.com/basics/dates-and-times/)