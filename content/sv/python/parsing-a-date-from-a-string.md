---
title:                "Att ta ut ett datum från en sträng"
html_title:           "Python: Att ta ut ett datum från en sträng"
simple_title:         "Att ta ut ett datum från en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Dateringsparsning i Python handlar om att koda för att extrahera ett datum från en sträng. Det är en vanlig uppgift för programmerare eftersom de ofta hanterar data som innehåller datuminformation. Genom att parsra datum från en sträng kan man enkelt omvandla det till ett format som är läsbart och hanterbart för datorn.

## Hur du gör:
För att parsra ett datum från en sträng i Python kan du använda funktionen "datetime.datetime.strptime ()". Här är ett kodexempel:

```Python
import datetime

datum_sträng = "12/01/2021"

datum = datetime.datetime.strptime(datum_sträng, "%d/%m/%Y")

print(datum)
```

Detta kommer att producera utmatningen "2021-01-12 00:00:00", som är ett datumobjekt som nu kan manipuleras och formateras enligt behov.

## Djupdykning:
Dateringsparsning har varit en utmaning för programmerare under lång tid, särskilt när det gäller hanteringen av olika format och regionalt specifika datumnoteringar. Tidigare krävde det ofta komplicerade och tidskrävande algoritmer för att parsra datum från strängar, men med funktionen "datetime.datetime.strptime()" är processen mycket enklare.

Det finns också andra sätt att parsra datum från en sträng i Python, såsom att använda modulen "dateutil.parser" eller regex. Valet av metod beror på dina behov och preferenser.

## Se även:
- [Python dokumentation om datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [dateutil.parser dokumentation](https://dateutil.readthedocs.io/en/stable/parser.html)
- [En tutorial om att parsra datum i Python](https://www.programiz.com/python-programming/datetime/strptime)