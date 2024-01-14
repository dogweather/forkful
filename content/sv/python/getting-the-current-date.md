---
title:                "Python: Hämta nuvarande datum"
simple_title:         "Hämta nuvarande datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför 

Att kunna få tag på den aktuella datum och tiden är en viktig aspekt av programmering. Det kan hjälpa dig i många situationer, till exempel när du vill hålla koll på när en viss händelse inträffade eller när du vill generera dynamiskt innehåll baserat på den aktuella tiden. I denna bloggartikel kommer vi att gå igenom hur man får tag på den aktuella datumet och tiden i Python-programmeringsspråket.

## Så här gör man

För att få den aktuella tiden i Python kan du använda funktionen datetime.now() från modulen datetime. För att använda denna funktion måste du först importera modulen genom att skriva ```Python import datetime``` i början av ditt program. Sedan kan du använda funktionen genom att skriva ```Python datetime.now()```. Om du bara vill ha datumet och inte tiden kan du använda funktionen date() istället för now(). Här är ett exempel på hur du kan använda dessa funktioner och vilken typ av utmatning du kan förvänta dig:

```Python
import datetime

nu = datetime.now()
datum = datetime.date()

print(nu)
print(datum)
```

Output:
```
2021-01-26 12:15:30.135473
2021-01-26
```

Som du kan se ger funktionen now() både datum och tid medan funktionen date() endast ger datumet. Om du vill ange ett specifikt datum eller tid istället för den aktuella tiden kan du använda funktionen datetime() och ange år, månad, dag, timme osv. som argument. Här är ett exempel på hur du kan använda det:

```Python
import datetime

datum = datetime.datetime(2020, 12, 31, 23, 59)

print(datum)
```

Output:
```
2020-12-31 23:59:00
```

Du kan också formatera utmatningen genom att använda formateringssträngar med funktionen strftime(). Här är ett exempel på hur en formateringssträng kan se ut och hur du använder den:

```Python
import datetime

datum = datetime.datetime.now()

print(datum.strftime("%d/%m/%y"))
```

Output:
```
26/01/21
```

## Djupgående

Nu när du vet hur du kan få tag på den aktuella tiden i Python, låt oss titta lite närmare på modulen datetime och dess funktioner.

DateTime-modulen innehåller flera andra användbara funktioner för datum och tidshantering. Här är några av dem:

- datetime.combine(): Kombinerar datum och tid till ett datetime-objekt.
- datetime.strptime(): Konverterar en sträng till ett datetime-objekt enligt ett visst datum- och tidsformat.
- date.weekday(): Ger veckodagen för ett visst datum (0 för måndag, 6 för söndag).

Du kan läsa mer om dessa och andra funktioner i det officiella Python-dokumentationen för datetime-modulen.

## Se även

- [Python's officiella klockdokumentation](https://docs.python.org/3/library/datetime.html)
- [En guide till datetime-modulen i Python](https://realpython.com/python-datetime/)
- [Allt du behöver veta om datum och tidshantering i Python](https://www.programiz.com/python-programming/datetime)