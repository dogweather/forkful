---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Datum till strängkonvertering är en process där en datums datastruktur ändras till en sträng. Detta gör det möjligt för programmerare att lagra, manipulera eller visa datum på ett enklare och mer läsbart format.

## Hur man Gör:
```Python
from datetime import date

# Skapa ett datum
d = date(2020, 8, 25)

# Konvertera datum till sträng
date_string = d.strftime("%d-%m-%Y")

print(date_string)  # Ger 25-08-2020
```
I det här explet har vi skapat ett datum-objekt och konverterat det till en sträng med platsmärken (%d för dag, %m för månad, %Y för år).

## Djupdykning
Konvertering av datum till sträng sträcker sig tillbaka till tidigare versioner av Python där att manipulera datum var mer komplicerat. Python 3.2 introducerade `strftime()` vilket gjorde processen enkel.

Alternativa metoder för att konvertera datum till sträng innefattar att använda externa bibliotek som `arrow` eller `pendulum`.

Vid implementering, kom ihåg att `strftime()` tolkar datum baserat på din lokala tidszon. Om du hanterar datum från olika tidszoner, var då noggrann när du gör omvandlingen.

## Se även
1. Officiell Python-dokumentation om datum och tid: https://docs.python.org/3/library/datetime.html
2. Python `strftime()`-guide: https://strftime.org/
3. Externa bibliotek för datumhantering: `arrow` (https://arrow.readthedocs.io/en/latest/) och `pendulum` (https://pendulum.eustace.io/)