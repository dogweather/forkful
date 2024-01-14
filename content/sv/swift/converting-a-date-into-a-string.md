---
title:    "Swift: Omvandla ett datum till en sträng"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att omvandla ett datum till en sträng är en vanlig uppgift inom Swift-programmering. Det kan vara användbart för att visa datum på ett mer läsbart sätt, skapa ett unikt ID eller kommunicera med en databas. Oavsett anledning, är det viktigt att förstå hur man gör denna konvertering korrekt.

## Hur man gör

```Swift
let date = Date()
let dateFormatter = DateFormatter()

dateFormatter.dateFormat = "dd-MM-yyyy" // bestämmer formatet på strängen
let dateAsString = dateFormatter.string(from: date) // omvandlar datumet till en sträng
print(dateAsString) // output: 17-07-2021
```

Det första steget är att skapa en instans av typen `Date` som representerar det aktuella datumet och tiden. Sedan skapar vi en instans av `DateFormatter` för att kunna formatera strängen enligt våra behov. Genom att ange det önskade formatet i `dateFormat` property, kan vi omvandla datumet till en sträng med hjälp av `string(from:)` metoden. Slutligen kan vi skriva ut den nya strängen för att se resultatet.

För att få en mer exakt strängrepresentation av datumet, kan man även använda `locale` och `timeZone` properties på `DateFormatter`.

## En djupdykning

Det är viktigt att tänka på att datum- och tidsformatet kan variera beroende på vilket språk och region användaren har på sin enhet. Därför är det alltid en god idé att använda `Locale` och `TimeZone` för att få en korrekt representation av datumet.

En annan viktig aspekt att tänka på är att omvandlingen från `Date` till sträng sker utifrån det aktuella systemets tidszon och inte nödvändigtvis den som är angiven i `DateFormatter`. Detta kan leda till fel i vissa fall, så se till att alltid ha rätt tidszon inställd innan du utför konverteringen.

## Se även

* [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
* [Swift by Sundell - Definitive guide to date and time formatting](https://www.swiftbysundell.com/articles/the-definitive-guide-to-date-and-time-formatting-in-swift/)