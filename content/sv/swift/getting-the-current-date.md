---
title:                "Hämta aktuellt datum"
html_title:           "Arduino: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Programmera med Swift: Så får du datumet för idag

## Vad & Varför?

Att få dagens datum är processen att hämta aktuellt datum och tid från systemet. Programmerare gör detta för att spåra händelseförlopp, logga data eller för rent visuella syften i en app.

## Så här gör du:

Det är inte svårt att få dagens datum i Swift. Här är ett exempel på hur du gör:

```Swift
let nu = Date()
print(nu)
```

Koden ovan kommer att producera något liknande följande utmatning:

```Swift
2023-02-01 14:37:42 +0000
```

Denna kod kommer att producera dagens datum och tid i GMT format.

## Djupdykning

Det har alltid varit viktigt för system och program att kunna hålla reda på tid och datum. I Swift, som släpptes av Apple 2014, kan detta uppnås med inbyggda metoder tillgängliga i `Date` klassen.

Alternativt kan du också använda `Calendar` klassen för mer komplexa datumoperationer. Till exempel:

```Swift
let nu = Date()
let kalender = Calendar.current
let komponenter = kalender.dateComponents([.year, .month, .day], from: nu)
```

Ovanstående kod kommer att ge dig året, månaden och dagen för dagens datum.

Däremot kan enklare användningar, som att helt enkelt få dagens datum, uppnås med en enda linje kod genom klassen `Date`.

## Se också:

- Apple officiella dokumentation om Swifts `Date` och `Calendar` klasser: 
  - [Date](https://developer.apple.com/documentation/foundation/date)
  - [Calendar](https://developer.apple.com/documentation/foundation/calendar)
- Swift programmeringsspråkets historia: [här](https://en.wikipedia.org/wiki/Swift_(programming_language)).
- Digital Trends djupdykningar i Swift: [här](https://www.digitaltrends.com/computing/what-is-swift/).