---
title:    "Python: Att hämta den aktuella datumet"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Varför

Att hämta och använda den aktuella datumen kan vara en användbar funktion inom programmering. Det kan hjälpa till att spåra när en viss kod utfördes, inkludera tidsstämplar i filnamn och mycket mer.

## Hur man gör det

För att hämta den aktuella datumet i Python, används en modul som heter `datetime`. Först måste du importera modulen i ditt program med hjälp av `import datetime`.

För att få den aktuella datumen, kan du använda följande kod:

```Python
import datetime

nuvarande_datum = datetime.date.today()
print(nuvarande_datum)
```

Det här kommer att ge dig en output som liknar `2021-05-10`. Om du också vill inkludera tiden, kan du använda `datetime.datetime.now()` istället för `datetime.date.today()`, vilket ger dig en output som ser ut som `2021-05-10 11:30:00.000000`.

Du kan också formatera outputen för att visas på ett specifikt sätt. Till exempel, om du vill ha datumet i formatet "DD/MM/YYYY", kan du använda följande kod:

```Python
import datetime

nuvarande_datum = datetime.date.today()
formaterat_datum = nuvarande_datum.strftime("%d/%m/%Y")
print(formaterat_datum)
```

Detta kommer att ge dig en output som ser ut som `10/05/2021`.

## Grundlig genomgång

Modulen `datetime` har flera funktioner för att hämta och manipulera datum och tid i Python. Här är några av de vanligaste funktionerna som används för att hämta den aktuella datumet:

- `datetime.date.today()` - Returnerar den aktuella datumen som ett `date` objekt.
- `datetime.datetime.now()` - Returnerar den aktuella datumen och tiden som ett `datetime` objekt.
- `datetime.datetime.strptime()` - Omvandlar en sträng till ett `datetime` objekt med en specifik formatering.
- `datetime.strftime()` - Formaterar ett `datetime` objekt till en sträng med en specifik formatering.

Du kan också använda `datetime` modulen för att manipulera datumen och tiden, till exempel genom att lägga till eller ta bort dagar, månader eller år. För mer detaljerad information kan du läsa dokumentationen för `datetime` modulen.

## Se även

- [Python:s officiella dokumentation för datetime](https://docs.python.org/3/library/datetime.html)
- [Enkel Python: Hämta den aktuella datumen och tiden](https://www.pythonforbeginners.com/basics/python-datetime-timedelta)
- [W3Schools: Python datetime modulen](https://www.w3schools.com/python/python_datetime.asp)