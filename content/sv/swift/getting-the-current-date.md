---
title:                "Hämta den aktuella datumet"
html_title:           "Swift: Hämta den aktuella datumet"
simple_title:         "Hämta den aktuella datumet"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kunna få den nuvarande datumet är en viktig del av programmering eftersom det är en vanlig uppgift som måste utföras. Många gånger behöver vi använda det aktuella datumet för att hantera tidsrelaterade uppgifter, som att visa datumet på ett användargränssnitt eller beräkna tidsskillnader mellan två datum.

## Så här gör du:
För att få den nuvarande datumet i Swift kan du använda "Date()" -funktionen. Detta skapar en instans av "Date" -typen som innehåller det aktuella datumet och tiden. Exempelvis:
```Swift
let currentDate = Date()
print(currentDate)
```
Detta kommer att skriva ut något i stil med "2021-11-11 14:27:30 +0000".

Om du vill formatera datumet på ett specifikt sätt kan du använda "DateFormatter" -klassen. Detta gör det möjligt att anpassa datum- och tidsformatet baserat på dina behov. Exempelvis:
```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
let currentDate = Date()
print(dateFormatter.string(from: currentDate))
```
Detta kommer att skriva ut dagens datum i formatet "11/11/2021".

## Djupdykning:
Historiskt sett var det mycket svårt att få den nuvarande datumet i programmeringsspråk, och det krävdes ofta mycket komplex kod. Men med införandet av inbyggda datum- och tidstyper i Swift har detta blivit mycket enklare och mer exakt.

Alternativ till att använda "Date()" är att använda tredjepartsbibliotek som "CoffeeScript", "SwiftDate" eller "Chronology". Dessa bibliotek ger mer avancerade funktioner för att hantera datum och tider, men de kräver extra installation och inlärning.

När det gäller implementation, använder "Date()" -funktionen en algoritm som bestämmer den nuvarande tidpunkten baserat på enhetens systemklocka. Det är dock viktigt att notera att om enhetens datum och tid inte är korrekt konfigurerad, kan detta påverka det aktuella datumet som returneras.

## Se även:
- [Apple Date Dokumentation](https://developer.apple.com/documentation/foundation/date)
- [CoffeeScript Bibliotek](https://github.com/hebertialmeida/CoffeeScript)
- [SwiftDate Bibliotek](https://github.com/malcommac/SwiftDate)
- [Chronology Bibliotek](https://github.com/davedelong/Chronology)