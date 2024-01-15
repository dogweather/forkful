---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Swift: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att räkna ut ett datum i framtiden eller förflutet kan vara användbart för att planera möten eller evenemang, eller för att hålla koll på deadlines. Det är också en bra övning för att lära sig hantera datum i Swift.

## Hur man gör

För att räkna ut ett datum i framtiden eller förflutet behöver du använda dig av Swifts inbyggda Calendar- och Date-klasser. Här är ett exempel på hur du kan beräkna ett datum som ligger en månad framåt:

```Swift
let calendar = Calendar.current
let today = Date()
let futureDate = calendar.date(byAdding: .month, value: 1, to: today)

print(futureDate ?? "No date found!")
```

I det här exemplet används `Calendar.current` för att hämta den aktuella kalendern och `Date()` för att få dagens datum som utgångspunkt. Sedan använder vi `calendar.date(byAdding:to:)` för att lägga till en månad till dagens datum och få det nya datumet som output.

För att räkna ut ett datum som ligger i förflutet behöver du bara byta ut `.month` till `.month` och ett negativt värde för `value` för att dra av istället för att lägga till.

## Fördjupning

När du räknar ut datum i Swift är det viktigt att förstå att det inte bara handlar om antalet dagar. Det finns många faktorer som påverkar beräkningarna, till exempel vilken tidszon som används och om det är skottår.

För att fördjupa dig ännu mer i hur datumberäkningar fungerar i Swift kan du läsa Swifts officiella dokumentation om Calendar och Date.

## Se även

- [Swift - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift - Date](https://developer.apple.com/documentation/foundation/date)