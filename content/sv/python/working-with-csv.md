---
title:                "Arbeta med csv"
html_title:           "Python: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-csv.md"
---

{{< edit_this_page >}}

## Vad & Varför?
CSV (Comma Separated Values) är ett vanligt filformat som används för att lagra strukturerad data i textform. Det är populärt bland programmerare eftersom det är lätt att läsa och behandla med kod. 

Förutom att vara lättläst av människor, är CSV också användbart eftersom det kan lagra olika datatyper, till exempel strängar och numeriska värden, i samma fil.

## Hur man gör:
Att arbeta med CSV-filer i Python är enkelt och smidigt. Först måste vi importera det inbyggda csv-biblioteket. Sedan öppnar vi vår CSV-fil med hjälp av functionen "open" och anger dess läge. Därefter använder vi "csv.reader" för att läsa filen rad för rad och lägga till datan i en lista.

```Python
import csv

with open('exempelfil.csv', 'r') as f:
    reader = csv.reader(f)
    for row in reader:
        print(row)
```

Output:
```Python
['Namn', 'E-post', 'Telefon']
['Sara', 'sara@mail.com', '0701234567']
['Anna', 'anna@mail.com', '0734567891']
```

## Deep Dive:
CSV-formatet skapades på 1970-talet för att underlätta datautbyte mellan olika system. Det finns dock alternativ till CSV, som till exempel JSON och XML, som är mer utvecklade och kan hantera mer komplex data.

För att skriva till en CSV-fil från Python använder man "csv.writer" istället för "csv.reader" och använder en annan lägeparameter för att skriva till filen. CSV-filer kan också ha kommatecken eller andra separatorer som skiljetecken och inte bara komma.

## Se även:
- [Python's official documentation on CSV](https://docs.python.org/3/library/csv.html)
- [A comparison of CSV vs JSON vs XML](https://www.computerhope.com/issues/ch001356.htm)
- [Using CSV files in Python, a tutorial by Real Python](https://realpython.com/python-csv/)