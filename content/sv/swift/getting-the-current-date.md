---
title:                "Swift: Att hämta aktuellt datum"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet är en viktig del av många Swift-program. Det är användbart för att dynamiskt skapa datumbaserade funktioner och för att hålla koll på tidsreferenser i en applikation.

## Hur man gör

För att hämta den nuvarande datumet i Swift, använder man sig av klassen Date och metoden Date().

```Swift
let nuvarandeDatum = Date()
print(nuvarandeDatum)
```

Output: 2021-03-12 17:00:00 +0000

I exemplet ovan skapar vi en konstant "nuvarandeDatum" som är av typen Date och tilldelar det värdet som returneras av metoden Date(). Sedan använder vi print() funktionen för att skriva ut värdet på skärmen.

Om man vill göra en anpassning av hur datumet visas, kan man använda sig av DateFormatter klassen. Detta gör det möjligt att formatera datumet på olika sätt, till exempel som ett kort datum eller ett datum med veckodag.

```Swift
let nuvarandeDatum = Date()
let formatter = DateFormatter()

// För kort datumformat
formatter.dateStyle = .short

// För datum med veckodag
formatter.dateStyle = .full

print(formatter.string(from: nuvarandeDatum))
```

Output för kort datumformat: 12/03/21

Output för datum med veckodag: fredag 12 mars 2021

## Djupdykning

Date klassen i Swift baseras på den C structen, time_t, som lagrar datumet och tiden i antalet sekunder sedan 1970-01-01 00:00:00 UTC. Detta är också känd som Unix-tiden. För att konvertera detta antal sekunder till ett datum, använder Swift en algoritm för att räkna ut det aktuella datumet baserat på antalet sekunder som har gått sedan dess.

Man kan också använda sig av metoden timeIntervalSince1970() för att få antalet sekunder som har gått sedan 1970. Detta är en användbar metod för att beräkna skillnaden mellan två datum eller för att skapa en timestamp.

## Se även

- [Apple dokumentation för Date klassen](https://developer.apple.com/documentation/foundation/date)
- [Guide till Swift Date and Time](https://www.iosapptemplates.com/blog/swift-programming/date-time)
- [Att arbeta med datum i Swift](https://medium.com/swift-india/working-with-date-in-swift-ii-f9a968975220)