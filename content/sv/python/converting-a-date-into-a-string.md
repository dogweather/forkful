---
title:                "Python: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att omvandla ett datum till en sträng är en vanlig uppgift i många Python-program. Det kan behövas för att skapa snygga utskrifter eller när man ska spara datum i en fil eller databas. 

## Så här gör du
För att konvertera ett datum till en sträng i Python behöver du först importera "datetime" biblioteket. Sedan kan du använda funktionen "strftime" för att välja det önskade strängformatet och sedan ange vilket datum som ska omvandlas.

```Python
# Importera datetime biblioteket
import datetime

# Skapa ett datumobjekt
datum = datetime.datetime(2021, 3, 19)

# Konvertera till sträng med formatet åååå-mm-dd
omvandlat_datum = datum.strftime("%Y-%m-%d")

# Skriv ut resultatet
print(omvandlat_datum)

# Resultat: 2021-03-19
```

Du kan också välja att använda andra format, till exempel "datummånadå" med "%d-%b-%Y".

```Python
# Importera datetime biblioteket
import datetime

# Skapa ett datumobjekt
datum = datetime.datetime(2021, 3, 19)

# Konvertera till sträng med formatet åååå-mm-dd
omvandlat_datum = datum.strftime("%d-%b-%Y")

# Skriv ut resultatet
print(omvandlat_datum)

# Resultat: 19-Mar-2021
```

## Djupdykning
När du konverterar ett datum till en sträng kan du också inkludera tid och tidszon. Det finns olika format som du kan använda beroende på hur du vill ha utskriften. Du kan också använda funktionen "strptime" för att omvandla en sträng till ett datum.

```Python
# Importera datetime biblioteket
import datetime

# Skapa ett datetime objekt
datum = datetime.datetime(2021, 3, 19, 13, 25)

# Konvertera till sträng med formatet åååå-mm-dd T hh:mm
omvandlad_strang = datum.strftime("%Y-%m-%d T %H:%M")

# Skriv ut resultatet
print(omvandlad_strang)

# Resultat: 2021-03-19 T 13:25

# Konvertera sträng till ett datetime objekt
nytt_datum = datetime.datetime.strptime("Mar 19, 2021", "%b %d, %Y")

# Skriv ut resultatet
print(nytt_datum)

# Resultat: 2021-03-19 00:00:00
```

Det finns också andra funktioner som kan hjälpa dig att hantera datum och tid i Python, som "date", "time" och "timedelta". Det är viktigt att läsa dokumentationen för att välja den bästa metoden för ditt syfte.

## Se även
- [Python datetime bibliotek](https://docs.python.org/3/library/datetime.html)
- [strftime och strptime](https://docs.python.org/3/library/datetime.html#strftime-and-strptime-behavior)