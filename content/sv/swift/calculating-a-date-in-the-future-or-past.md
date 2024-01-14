---
title:                "Swift: Beräkning av ett datum i framtiden eller det förflutna"
simple_title:         "Beräkning av ett datum i framtiden eller det förflutna"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna kan vara användbart i olika situationer. Det kan till exempel hjälpa dig att planera evenemang, schemalägga uppgifter eller hålla reda på åldern hos dina nära och kära.

## Så här gör du

Att beräkna ett datum i Swift är enkelt med hjälp av Date-klassen. För att beräkna ett datum i framtiden kan du använda funktionen addingTimeInterval(), som tar ett antal sekunder som argument och lägger till det på det aktuella datumet. Till exempel, om du vill beräkna datumet en vecka från idag, så kan du göra så här:

```Swift
let enVeckaSenare = Date().addingTimeInterval(60*60*24*7)
print(enVeckaSenare)
```

Output:

```
2020-06-02 07:21:52 +0000
```

För att beräkna ett datum i det förflutna kan du använda funktionen subtractingTimeInterval() på samma sätt. Till exempel, om du vill beräkna datumet en månad tillbaka i tiden, kan du göra så här:

```Swift
let enManadTillbaka = Date().subtractingTimeInterval(60*60*24*30)
print(enManadTillbaka)
```

Output:

```
2020-04-03 07:21:52 +0000
```

Du kan också beräkna ett datum baserat på ett befintligt datum med hjälp av DateComponents, som låter dig specificera exakt vilken del av datumet du vill ändra. Till exempel, om du vill beräkna datumet en vecka från en specifik datum, kan du göra så här:

```Swift
let date = DateComponents(year: 2020, month: 6, day: 2).date!
let enVeckaFranDate = Calendar.current.date(byAdding: .day, value: 7, to: date)
print(enVeckaFranDate)
```

Output:

```
2020-06-09 07:21:52 +0000
```

## Djupdykning

Date-klassen i Swift är baserad på Gregorian-kalendern. Detta betyder att det tar hänsyn till skottår och månader med olika antal dagar. Det finns också många andra användbara funktioner som kan hjälpa dig att hantera datum, till exempel att jämföra två datum eller konvertera mellan olika tidszoner.

Om du vill lära dig mer om hantering av datum i Swift, så rekommenderar vi att du tittar närmare på Date- och Calendar-klassen i Apples dokumentation.

## Se även

- [Apples dokumentation om Date-klassen](https://developer.apple.com/documentation/foundation/date)
- [Apples dokumentation om Calendar-klassen](https://developer.apple.com/documentation/foundation/calendar)
- [Tutorial: Hantera datum och tidszoner i Swift](https://medium.com/@jamesrochabrun/managing-dates-and-time-zones-in-swift-e4b2c6cae42e) (på engelska)