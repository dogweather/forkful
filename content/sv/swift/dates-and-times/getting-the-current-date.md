---
title:                "Få det aktuella datumet"
date:                  2024-02-03T19:11:01.185838-07:00
model:                 gpt-4-0125-preview
simple_title:         "Få det aktuella datumet"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att få det aktuella datumet i Swift innebär att använda `Date`-klassen för att få tillgång till datum och tid när appen körs. Programmerare behöver hämta det aktuella datumet av många olika skäl, från att tidstämpla händelser, utföra datumberäkningar, till att visa datum och tider i ett användargränssnitt.

## Hur man gör:
Swifts `Foundation`-ramverk tillhandahåller `Date`-klassen, vilket gör det enkelt att få det aktuella datumet och tiden. Här är ett grundläggande exempel på hur du får det aktuella datumet:

```swift
import Foundation

let currentDate = Date()
print(currentDate)
```

Detta kommer att producera något liknande:

```
2023-04-12 07:46:23 +0000
```

Utdataformatet följer ISO 8601-standarden, med UTC-tidszonen. Dock kanske du vill formatera detta datum för visningsändamål. Swifts `DateFormatter`-klass kommer till undsättning:

```swift
let formatter = DateFormatter()
formatter.dateStyle = .long
formatter.timeStyle = .medium
let formattedDate = formatter.string(from: currentDate)
print(formattedDate)
```

Ett exempel på utdata kan vara:

```
12 april 2023 kl. 10:46:23
```

Notera att utdataformatet kommer att variera beroende på enhetens lokala inställningar som kör koden.

För projekt som kräver mer komplex datummanipulation vänder sig många Swift-utvecklare till tredjepartsbibliotek såsom `SwiftDate`. Så här kan du använda `SwiftDate` för att få det aktuella datumet i en specifik tidszon och format:

Först, lägg till `SwiftDate` i ditt projekt med SPM, CocoaPods eller Carthage. Sedan:

```swift
import SwiftDate

let rome = Region(calendar: .gregorian, zone: .europeRome, locale: .current)
let currentDateInRome = DateInRegion(Date(), region: rome)
print(currentDateInRome.toFormat("yyyy-MM-dd HH:mm:ss"))
```

Detta kunde producera:

```
2023-04-12 09:46:23
```

Med hjälp av `SwiftDate` kan du enkelt manipulera datum och tider för olika tidszoner och platser, vilket förenklar komplexa datumhanteringsuppgifter i dina Swift-applikationer.
