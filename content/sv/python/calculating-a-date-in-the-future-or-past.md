---
title:                "Beräkna ett datum i framtiden eller det förflutna"
html_title:           "Python: Beräkna ett datum i framtiden eller det förflutna"
simple_title:         "Beräkna ett datum i framtiden eller det förflutna"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller förfluten tid är en nyttig färdighet inom programmering, speciellt när man arbetar med tidskänsliga uppgifter eller behöver automatiskt generera datum för program.

## Så här

För att göra detta i Python, kan vi använda de inbyggda biblioteken "datetime" och "timedelta". För att beräkna ett datum i framtiden, kan vi använda funktionen "today()" för att hämta dagens datum, och sedan lägga till ett antal dagar med "timedelta(days=x)" funktionen. Här är ett exempel:

```Python
import datetime

dagens_datum = datetime.date.today()
framtida_datum = dagens_datum + datetime.timedelta(days=7)

print(framtida_datum)

# Output: 2021-03-23 
```

För att beräkna ett datum i förfluten tid, kan vi använda samma princip, men med funktionen "timedelta(days=-x)". Här är ett exempel:

```Python
import datetime

dagens_datum = datetime.date.today()
förflutna_datum = dagens_datum + datetime.timedelta(days=-7)

print(förflutna_datum)

# Output: 2021-03-09
```

## Djupdykning

Datumanpassning kan också göras för år, månader, timmar, minuter och sekunder. Till exempel, för att beräkna ett datum som är en månad framåt, kan vi använda "timedelta(months=1)". Detta är särskilt användbart när man arbetar med komplexa datum- och tidsberäkningar.

En annan användbar funktion är "strftime()" som kan användas för att formatera datum och tid i olika format. Här är ett exempel:

```Python
import datetime

datum = datetime.date(2021, 3, 16)

# Formatera datumet i formatet DDMÅNÅR
formaterat_datum = datum.strftime("%d%m%Y")

print(formaterat_datum)

# Output: 16032021
```

## Se även

- [Python dokumentation om datetime](https://docs.python.org/3/library/datetime.html)
- [Python Timedelta referens](https://docs.python.org/3/library/datetime.html#timedelta-objects)