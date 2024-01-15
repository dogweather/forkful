---
title:                "Jämföra två datum"
html_title:           "Python: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart för att se skillnaden mellan dem, till exempel hur många dagar det är mellan dem eller om ett datum kommer före eller efter det andra. Det kan också vara användbart när man arbetar med tidsbaserade data eller behöver utföra beräkningar baserade på datum.

## Så här gör du
För att jämföra två datum i Python kan du använda modulen "datetime". Först måste du importera modulen:

```Python
import datetime
```

Sedan kan du skapa två datetime-objekt med de önskade datumen:

```Python
date1 = datetime.datetime(2020, 1, 1)
date2 = datetime.datetime(2021, 1, 1)
```

För att jämföra dessa två datum kan du använda jämförelseoperatorerna "<", "<=", ">", ">=" eller "==".

```Python
print(date1 < date2) # True
print(date1 == date2) # False
print(date1 > date2) # False
```

Du kan också få ut skillnaden mellan de två datumen i dagar genom att subtrahera dem och använda ".days" på resultatet:

```Python
print((date2 - date1).days) # 366
```

## Djupdykning
När du jämför två datum måste de vara av typen "datetime". Om du har datum som strings eller i annat format måste du först konvertera dem till datetime-objekt. Du kan använda "strptime" för att konvertera från string till datetime eller "strftime" för att konvertera från datetime till string.

En vanlig fallgrop när man jämför datum är tidszoner. Ett datum som är i en annan tidszon kan resultera i en felaktig jämförelse. Se till att ange rätt tidszon när du skapar datetime-objekt för att undvika detta.

## Se även
- [Python datetime-modulen](https://docs.python.org/3/library/datetime.html)
- [Jämförelseoperatorer i Python](https://www.w3schools.com/python/python_operators.asp)
- [Python strptime() och strftime() för datumkonvertering](https://www.programiz.com/python-programming/datetime/strftime)