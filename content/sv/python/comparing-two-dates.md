---
title:                "Python: Jämföra två datum"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum kan vara användbart i många situationer, exempelvis när du vill kontrollera om ett event har redan ägt rum eller när du vill sortera en lista med datum i kronologisk ordning.

## Hur man gör det
Först och främst måste du importera datetime-modulen i Python. Sedan kan du skapa två datumobjekt och jämföra dem genom att använda olika operatorer, till exempel >, < eller ==. Här är ett exempel på kod som jämför två datum och skriver ut resultatet:

```Python
import datetime

# Skapa två datumobjekt
datum1 = datetime.date(2021, 11, 15)
datum2 = datetime.date(2021, 11, 20)

# Jämför datum och skriv ut resultatet
if datum1 > datum2:
    print("Datum 1 är senare än datum 2.")
elif datum1 < datum2:
    print("Datum 2 är senare än datum 1.")
else:
    print("Datum 1 och datum 2 är samma datum.")
```

Output:
```
Datum 2 är senare än datum 1.
```

## Djupdykning
När du jämför två datum i Python, används operatorerna för större än (>), mindre än (<) och lika med (==) för att jämföra år, månader och dagar i respektive datum. Detta innebär att om år och månader är samma, men dagarna skiljer sig åt, kommer dagarna att avgöra vilket datum som anses vara större eller mindre.

Du kan också jämföra datum och tider med hjälp av datetime-objektet. På samma sätt som med datum, kan du använda olika operatorer för att jämföra tider, till exempel >, < eller ==. Se till att importera datetime-modulen och skapa separata datetime-objekt för att kunna jämföra tider.

## Se även
- [Officiell dokumentation för datetime-modulen i Python](https://docs.python.org/3/library/datetime.html)
- [En handledning för att arbeta med datum och tider i Python](https://realpython.com/python-datetime/)
- [En guide för operatorer i Python](https://www.programiz.com/python-programming/operators)