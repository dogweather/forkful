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

## Varför

Vi använder CSV-filer för att lagra och hantera stora mängder tabulär data på ett strukturerat sätt. Det är ett vanligt format som används för att dela data mellan olika applikationer och system.

## Så här gör du

För att arbeta med CSV-filer i Python behöver du importera `csv`-modulen. Här är ett kort kodexempel som läser in en CSV-fil och skriver ut varje rad i konsolen:

```Python
import csv

with open('data.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    for row in csv_reader:
        print(row)
```

Output:

``` 
['Namn', 'Ålder', 'Stad']
['Lisa', '25', 'Stockholm']
['Erik', '30', 'Göteborg']
['Maria', '27', 'Malmö']
```

För att skriva till en CSV-fil kan du använda `writer`-funktionen från `csv`-modulen. Se till att du öppnar filen i läge "append" (`a`) om du vill lägga till data till en befintlig fil.

```Python
import csv

with open('data.csv', 'a') as csv_file:
    csv_writer = csv.writer(csv_file)

    # Skriv en rad i taget
    csv_writer.writerow(['Johan', '32', 'Uppsala'])

    # Skriv flera rader på en gång
    data = [
        ['Alina', '29', 'Norrköping'],
        ['Oskar', '24', 'Lund']
    ]
    csv_writer.writerows(data)
```

Output i data.csv:

```
Namn,Ålder,Stad
Lisa,25,Stockholm
Erik,30,Göteborg
Maria,27,Malmö
Johan,32,Uppsala
Alina,29,Norrköping
Oskar,24,Lund
```

## Djupdykning

CSV står för "Comma Separated Values" och det är just det - ett textbaserat format där varje rad med data separeras av ett kommatecken. I vissa fall används även andra separatorer, till exempel semikolon eller tabb, men i det här fallet kommer vi att fokusera på den vanligaste formen med kommatecken.

Förutom att läsa och skriva data till CSV-filer, kan vi även manipulera data med hjälp av inbyggda funktioner som `split()` och `join()`. Till exempel:

```Python
import csv

with open('data.csv', 'r') as csv_file:
    csv_reader = csv.reader(csv_file)

    for row in csv_reader:
        # Dela upp namnet i för- och efternamn
        name = row[0].split()
        first_name = name[0]
        last_name = name[1]

        # Skapa ett användarnamn genom att sammanslå första bokstaven av förnamn med hela efternamnet
        username = first_name[0] + last_name

        # Spara det som en ny kolumn i varje rad
        row.append(username)

        print(row)
```

Output:

```
['Namn', 'Ålder', 'Stad', 'Användarnamn']
['Lisa Andersson', '25', 'Stockholm', 'Landersson']
['Erik Svensson', '30', 'Göteborg', 'Esvensson']
['Maria Karlsson', '27', 'Malmö', 'Mkarlsson']
```

Det finns även möjlighet att läsa in CSV-data som en ordnad dictionary istället för en lista, genom att använda `DictReader`-funktionen. Detta gör det lättare att arbeta med data eftersom varje kolumn då blir en "nyckel" i dictionaryn.

```Python
import csv

with open('data.csv', 'r') as csv_file:
    csv_reader = csv.DictReader(csv_file)

    for row in csv_reader:
        print(row['Namn'] + " bor i " + row['Stad'])
```

Output:

```
Lisa bor i Stockholm
Erik bor i Göteborg
Maria bor i Malmö
```

## Se även

- [Officiell dokumentation för CSV i Python](https://docs.python.org/3/library/csv.html)
- [Lista med CSV-moduler för Python