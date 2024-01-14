---
title:                "Python: Arbeta med csv"
simple_title:         "Arbeta med csv"
programming_language: "Python"
category:             "Python"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/working-with-csv.md"
---

{{< edit_this_page >}}

# Varför

CSV, eller "Comma-Separated Values", är ett vanligt filformat som används för att lagra data i tabellform. Det är ett praktiskt sätt att hantera data eftersom det är lätt att läsa och hantera för både människor och datorer. Om du arbetar med dataanalys, webbutveckling eller automatisering kan du ofta stöta på CSV-filer och det är därför viktigt att ha kunskap om hur du kan jobba med dem.

# Hur man gör

Att arbeta med CSV-filer i Python är relativt enkelt tack vare många inbyggda funktioner och moduler. För att öppna och läsa en CSV-fil kan du använda funktionen `open()` och modulen `csv`. Se nedan för ett exempel på hur du kan läsa en CSV-fil och skriva ut dess innehåll:

```python
# Importera csv-modulen
import csv

# Öppna csv-filen och läs in data
with open('exempel.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    # Loopa genom varje rad i filen
    for row in csv_reader:
        # Skriv ut varje rad som en lista
        print(row)
```

Detta kodblock kommer att skriva ut innehållet i filen `exempel.csv` som en lista av listor, där varje inre lista representerar en rad i filen.

Om du vill skriva till en CSV-fil kan du använda funktionen `writer()` från `csv`-modulen. Se nedan för ett exempel på hur du kan skapa en ny CSV-fil och skriva till den:

```python
# Importera csv-modulen
import csv

# Skapa en ny csv-fil
with open('ny_fil.csv', mode='w') as csv_file:
    # Skapa en csv-writer
    csv_writer = csv.writer(csv_file, delimiter=',')
    # Skriv till filen
    csv_writer.writerow(['Förnamn', 'Efternamn', 'Ålder'])
    csv_writer.writerow(['Anna', 'Andersson', 30])
    csv_writer.writerow(['Erik', 'Ek', 25])
```

Detta kodblock kommer att skapa en ny fil med namnet `ny_fil.csv` och skriva innehållet som en tabell med tre kolumner: "Förnamn", "Efternamn" och "Ålder".

# Djupdykning

Att läsa och skriva CSV-filer är bara en del av vad du kan göra med dem i Python. Du kan också använda modulen `csv` för att manipulera data, såsom att lägga till eller ta bort rader och kolumner, eller för att jämföra data mellan olika filer.

En annan användbar modul för att arbeta med CSV-filer är `pandas`, som är speciellt utformad för dataanalys. Med `pandas` kan du enkelt läsa in och manipulera stora mängder CSV-data och utföra olika typer av analyser.

# Se även

* [Officiell dokumentation för csv-modulen](https://docs.python.org/3/library/csv.html)
* [Pandas dokumentation](https://pandas.pydata.org/pandas-docs/stable/)
* [En artikel om dataanalyser med pandas](https://realpython.com/python-data-analysis/)