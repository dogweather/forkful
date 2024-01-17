---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Swift: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Beräkning av ett datum i framtiden eller det förflutna är en vanlig uppgift som många programmerare stöter på. Det är processen att använda ett startdatum och en tidsperiod för att generera ett nytt datum i relation till det ursprungliga.

Detta behövs ofta inom applikationsutveckling, särskilt för att hantera scheman och händelser. Genom att kunna beräkna datum i framtiden eller förflutna kan vi skapa mer dynamiska och anpassningsbara program.

## Så här:

Enklaste sättet att beräkna ett datum i framtiden eller förflutna är att använda metoden `addingTimeInterval` från `Date`-klassen. Nedan är ett exempel på hur du kan använda denna metod för att få ett datum 30 dagar från det aktuella datumet.

```Swift
let currentDate = Date()
let thirtyDaysFromNow = currentDate.addingTimeInterval(30*24*60*60) // 30 dagar har 24 timmar, 60 minuter och 60 sekunder
print(thirtyDaysFromNow) 

// Output:
// 2021-05-07 01:45:36 +0000
```

Det finns också andra metoder för att beräkna datum i framtiden eller förflutna såsom `addingWeekOfYear` och `addingMonth` som kan vara praktiska beroende på din specifika användning.

## Djupt dyk

Att kunna beräkna datum i framtiden eller förflutna har blivit enklare med utvecklingen av programmeringsspråk och ramverk. Förr i tiden, när de flesta enheter inte var anslutna till internet, var det vanligt att programmerare behövde skriva mer komplexa algoritmer för att göra dessa beräkningar.

Alternativa sätt att beräkna datum inkluderar användning av bibliotek såsom `DateUtils` som tillhandahåller mer avancerade funktioner för att manipulera datum. Men för enkelheten, är det fortfarande vanligt att använda inbyggda metoder som `addingTimeInterval`.

Implementeringen av dessa beräkningsfunktioner brukar använda sig av Gregoriankalendern som är den mest använda kalendern i världen. Men det finns också metoder som kan använda andra kalendrar beroende på ditt behov.

## Se även

Om du vill lära dig mer om hur du beräknar datum i framtiden eller förflutna, kan du besöka Apples dokumentation för `Date`-klassen och utforska de olika metoderna som finns tillgängliga. Du kan också läsa på om andra kalendrar och hur de kan användas i programmeringssammanhang.

Källor:
- [Apple Documentation - Date](https://developer.apple.com/documentation/foundation/date) 
- [DateUtils Library](https://github.com/malcommac/DateUtils) 
- [Gregorian Calendar](https://en.wikipedia.org/wiki/Gregorian_calendar)