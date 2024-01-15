---
title:                "Jämföra två datum"
html_title:           "Kotlin: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/kotlin/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Om du någonsin har behövt jämföra två olika datum i ditt kodprojekt, har du kommit till rätt ställe. Att kunna jämföra datum är en vanlig uppgift inom programmering och kan vara användbart för många olika typer av applikationer.

## Så här gör du

Om du vill jämföra två datum i Kotlin finns det flera sätt att göra det, beroende på vad du vill uppnå. Här är några exempel som visar hur man kan använda Kotlin för att jämföra datum:

```Kotlin
// Jämför två datum
val date1 = LocalDate.of(2021, 10, 1)
val date2 = LocalDate.of(2021, 9, 15)
println(date1 > date2) // true, eftersom 1 oktober kommer efter 15 september

// Jämför om två datum är lika
val date3 = LocalDate.of(2021, 12, 25)
val date4 = LocalDate.of(2021, 12, 25)
println(date3 == date4) // true, eftersom båda är 25 december

// Jämför om ett datum kommer före ett annat datum
val date5 = LocalDate.of(2021, 6, 30)
val date6 = LocalDate.of(2022, 1, 1)
println(date5 < date6) // true, eftersom 30 juni kommer före 1 januari

// Jämför datum med tidszoner
val date7 = ZonedDateTime.of(2021, 8, 1, 12, 0, 0, 0, ZoneId.of("Europe/Stockholm"))
val date8 = ZonedDateTime.of(2021, 8, 1, 12, 0, 0, 0, ZoneId.of("America/New_York"))
println(date7 == date8) // true, eftersom båda är 1 augusti klockan 12:00, men med olika tidszoner
```

Som du kan se i exemplen ovan använder vi olika metoder baserade på vilket typ av jämförelse vi vill göra. I det första exemplet använder vi `>` operatören för att jämföra två datum och se om det första datumet kommer efter det andra datumet. I det andra exemplet använder vi `==` operatören för att se om två datum är lika. I det tredje exemplet använder vi `<` operatören för att se om det första datumet kommer före det andra datumet. Slutligen i det fjärde exemplet använder vi `==` operatören för att jämföra två datum med olika tidszoner.

## Djupdykning

För de som är intresserade av att gå djupare in i ämnet, finns det flera viktiga saker att notera när man jämför datum i Kotlin:

- Kotlin använder sig av `Comparable` interfacet för att tillåta jämförelse mellan två objekt av samma typ. Eftersom `LocalDate` och `ZonedDateTime` implementerar detta interface, kan vi enkelt använda jämförelseoperatorerna som vi såg i exemplen ovan.

- Om vi vill jämföra om två datum är lika, kan vi också använda metoden `isEqual()` som tillhandahålls av Kotlin. Detta gör samma sak som `==` operatören men är mer explicit och kan vara mer läsbart i vissa fall.

- När vi jämför datum baserade på tidszoner, är det viktigt att förstå att ett datum i en viss tidszon kan vara annorlunda än en annan tidszon. Detta kan leda till skilda resultat vid jämförelse, beroende på vilken tidszon som används.

## Se även

- [Java 8 Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Kotlin Comparison and Equality](https://kotlinlang.org/docs/comparison.html)
- [Kotlin Comparable interface](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-comparable/index.html)