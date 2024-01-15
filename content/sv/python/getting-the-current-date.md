---
title:                "Att få den nuvarande datumen"
html_title:           "Python: Att få den nuvarande datumen"
simple_title:         "Att få den nuvarande datumen"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den aktuella datumet är en vanlig uppgift när man jobbar med Python-programmering. Det kan vara användbart för att spåra datumet för en händelse, göra tidstämplar för loggning eller helt enkelt visa den aktuella tiden i ditt program.

## Så här gör du

För att få den aktuella datumet i Python använder vi modulen `datetime`. Här är ett enkelt exempel på hur du kan få den aktuella datumet och skriva ut det i konsolen:

```Python 
import datetime

aktuell_datum = datetime.date.today()

print("Idag är det", aktuell_datum)
```

Output: `Idag är det 2020-08-20`

Du kan också anpassa hur datumet visas genom att använda metoder som `strftime()` och ange det önskade formatet. Här är ett exempel på hur du kan få datumet att visas som en sträng med formatet DD/MM/ÅÅÅÅ:

```Python
import datetime

aktuell_datum = datetime.date.today()

formaterat_datum = aktuell_datum.strftime("%d/%m/%Y")

print("Idag är det", formaterat_datum)
```

Output: `Idag är det 20/08/2020`

Det finns många andra metoder och funktioner i `datetime`-modulen som du kan utforska för att få den aktuella datumet på olika sätt.

## Djupdykning

När vi använder `datetime`-modulen för att få den aktuella datumet, använder vi faktiskt en klass som kallas `date`. En klass är ett blåtryck eller en mall för att skapa objekt. Med andra ord skapar vi ett objekt av klassen `date` när vi använder `datetime.date.today()`.

Klassen date har attribut som `year`, `month` och `day` som ger specifik information om datumet. Det finns också metoder som `weekday()` som ger vilken veckodag datumet faller på.

Om du vill utforska mer om hur `datetime`-modulen fungerar och vilka andra möjligheter och funktioner det finns, kan du läsa mer i den officiella dokumentationen: https://docs.python.org/3/library/datetime.html

## Se även

- https://www.geeksforgeeks.org/python-datetime-module-with-examples/
- https://realpython.com/python-datetime/
- https://www.programiz.com/python-programming/datetime