---
title:                "Swift: Att få den aktuella datumet"
simple_title:         "Att få den aktuella datumet"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna få den nuvarande datumen är en grundläggande funktion i många programmeringsspråk. I Swift använder vi Date-klassen för att göra detta möjligt. Det är viktigt att kunna få den nuvarande datumen för att kunna implementera funktioner som tidsstämplar och schema för att hålla koll på tid relaterade händelser. Det är också en viktig del av att kunna utföra datummanipulationer och tidsberäkningar.

## Så här

I Swift finns det flera sätt att få den nuvarande datumen, beroende på vad ditt specifika behov är.

Först och främst, om du bara behöver den nuvarande datumen med tidszoninställningar och sekundsprecision, kan du helt enkelt använda Date-klassen och dess initieringsmetod, som visas nedan i Swift-kodblocket:

```Swift
let currentDate = Date()
print(currentDate)
```

Detta kommer att ge dig utmatningen av den nuvarande datumen i standard UTC-tidszon och i formatet "ÅÅÅÅ-MM-DD HH:MM:SS +TTTT" där "TTTT" representerar tidsskillnaden från UTC i timmar och minuter.

Om du vill anpassa utmatningen, som att visa datumet i en annan tidszon eller ett annat format, kan du använda DateFormatter-klassen. I följande kodblock visas hur du kan få datumet i lokaltidszonen och i årtal, månad och dag format.

```Swift
let dateFormatter = DateFormatter()
dateFormatter.timeZone = TimeZone.current
dateFormatter.dateFormat = "yyyy-MM-dd"
let today = Date()
let localDate = dateFormatter.string(from: today)
print(localDate)
```

Detta kommer att ge dig utmatningen av den nuvarande datumen i formatet "ÅÅÅÅ-MM-DD".

## Djupdykning

De Date-klasser och initieringsmetoder som används för att få den nuvarande datumen är bara några av de funktioner som kan hjälpa dig att hantera datum och tid i Swift. Det finns andra klasser som Calendar och DateComponents som är användbara för att göra beräkningar och manipulationer på datumen. Du kan också utforska olika formateringsalternativ för datum och tid med DateFormatter-klassen.

## Se även

- [Swift - Date Class Documentation](https://developer.apple.com/documentation/foundation/date)
- [Swift - DateFormatter Class Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift - Calendar Class Documentation](https://developer.apple.com/documentation/foundation/calendar)
- [Swift - DateComponents Class Documentation](https://developer.apple.com/documentation/foundation/datecomponents)