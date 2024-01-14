---
title:                "Swift: Beräkna ett datum i framtiden eller i det förflutna"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Varför
Att kunna räkna ut ett datum i framtiden eller förflutna kan vara användbart i många situationer. Det kan hjälpa till att planera evenemang, hålla koll på födelsedagar eller bara få en bättre förståelse för tiden.

# Hur du gör det
För att räkna ut ett datum i Swift, kan du använda dig av datumaritmetiken som finns tillgänglig i `Foundation`-ramverket. Här är en enkel kodexempel på hur du kan räkna ut ett datum i framtiden:

```Swift
let today = Date()
let dateComponents = DateComponents(year: 2022, month: 10, day: 30)
let futureDate = Calendar.current.date(byAdding: dateComponents, to: today)

print(futureDate)
```

I detta exempel skapar vi först ett `Date`-objekt för dagens datum. Sedan använder vi `DateComponents` för att specificera detaljerna för det datumet som vi vill räkna ut i framtiden, i detta fall 30 oktober 2022. Slutligen använder vi `Calendar` för att lägga till dessa komponenter till dagens datum och få ut ett nytt `Date`-objekt för framtiden.

Output: 2022-10-30 09:00:00 +0000

Detta är bara ett enkelt exempel, men genom att leka runt med olika värden och använda olika metoder som `date(bySetting:to:)` eller `date(byAdding:to:)` kan du skapa kod som passar dina specifika behov.

# Djupdykning
Att räkna ut datum i framtiden eller förflutna kan ibland vara mer komplicerat än bara att lägga till eller dra bort ett antal månader, veckor eller dagar. Det kan också involvera saker som tidszoner och kalendrar.

Till exempel, om du vill räkna ut ett datum i en annan tidszon, så behöver du först konvertera ditt `Date`-objekt till rätt tidszon. Du kan använda dig av `TimeZone`-objekt och `DateFormatter` för att göra detta.

När det kommer till kalendrar, så är det viktigt att förstå att olika kulturer och religioner har olika kalendrar. I Swift finns det färdigbyggda kalendrar för de vanligaste kulturerna, men det är också möjligt att skapa egna kalendrar för mer specifika behov.

# Se även
Om du vill lära dig mer om hur man räknar ut datum i framtiden eller förflutna i Swift, så kan du läsa mer i dokumentationen för Foundation-ramverket och titta på olika kodexempel och guider online.

- Läs mer om DateComponents och Calendar i Foundation-dokumentationen: https://developer.apple.com/documentation/foundation/
- Se kodexempel och guider från andra Swift-utvecklare på GitHub: https://github.com/search?q=Swift+date+calculation