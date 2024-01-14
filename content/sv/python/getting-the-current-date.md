---
title:                "Python: Få den aktuella datumet"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumen i din kod kan verka som en enkel uppgift, men det finns faktiskt flera olika metoder för att göra det. Ofta behövs det för att spåra tid för olika processer eller för att ange en tidsstämpel för en viss händelse. Det kan också vara användbart för att visa information till användare, som till exempel när ett visst dokument skapades eller när en viss transaktion ägde rum.

## Hur man gör det

Att få det nuvarande datumet i Python är en ganska enkel uppgift. Här nedanför visas två olika metoder för att göra det.

```Python
# Först importeras datetime-modulen
import datetime

# Metod 1: Använda datetime.modulen
current_date = datetime.datetime.now()
print(current_date)

# Metod 2: Använda time.modulen
import time
current_date = time.strftime("%Y-%m-%d %H:%M:%S", time.localtime())
print(current_date)
```

Outputen kommer se ut så här:

> 2020-02-15 20:30:00

Det finns också flera olika sätt att formatera datumet och tiden på, beroende på vilken information du vill ha med. Här är en tabell som visar de vanligaste formateringsalternativen:

| Kort form | Lång form | Beskrivning                                        |
| --------- | ----------| -------------------------------------------------- |
| `%d`      | `%A`      | Dag i månaden                                       |
| `%m`      | `%B`      | Månaden som en nummer                             |
| `%y`      | `%Y`      | Året i två eller fyra siffror                        |
| `%H`      | `%I`      | Timme (24-timmarsformat eller 12-timmarsformat)      |
| `%M`      | `%S`      | Minut                                               |
| `%p`      | `%p`      | AM/PM                                              |

Du kan kombinera dessa olika formateringar för att visa det datum- och tidsformat du vill ha.

## Fördjupning

Det finns en hel del att utforska när det gäller att få det nuvarande datumet i Python. En intressant funktion är möjligheten att lägga till eller subtrahera dagar, månader eller år från det nuvarande datumet.

```Python
import datetime

# Lägger till 10 dagar till det nuvarande datumet
new_date = datetime.datetime.now() + datetime.timedelta(days=10)

print(new_date)
```

Detta kommer att ge följande output:

> 2020-02-25 20:30:00

Det är också möjligt att jämföra olika datum med varandra för att se vilket som är tidigare eller senare. Detta kan vara användbart för att utröna skillnaden i dagar mellan två specifika datum.

## Se också

- [Python datetime module documentation](https://docs.python.org/3/library/datetime.html)
- [Python time module documentation](https://docs.python.org/3/library/time.html)