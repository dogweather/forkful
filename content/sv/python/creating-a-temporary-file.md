---
title:                "Skapa en tillfällig fil"
html_title:           "Python: Skapa en tillfällig fil"
simple_title:         "Skapa en tillfällig fil"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför

Innan vi börjar, låt oss ta en snabb titt på varför vi behöver använda temporära filer i vår Python-kod. En temporär fil är en fil som skapas tillfälligt för att lagra data eller utföra en viss uppgift under exekveringen av vårt program. Detta kan vara användbart när vi arbetar med stora datamängder eller när vi behöver skriva till en fil utan att permanent lagra den på disk.

## Så här gör du

Att skapa en temporär fil i Python är en enkel process. Vi kan använda modulen `tempfile` för att skapa vår temporära fil och senare använda den för vår önskade uppgift. Först importerar vi modulen:

```Python
import tempfile
```

Nästa steg är att använda funktionen `NamedTemporaryFile()` för att skapa vår temporära fil och tilldela den till en variabel:

```Python
temp_file = tempfile.NamedTemporaryFile()
```

Om vi vill ange ett annat namn på vår temporära fil kan vi använda parametern `prefix`:

```Python
temp_file = tempfile.NamedTemporaryFile(prefix="my_temp_file")
```

Nu kan vi skriva in data i vår temporära fil precis som vi skulle göra med en vanlig fil:

```Python
temp_file.write("Det här är en temporär fil")
```

Vi kan också läsa från vår temporära fil:

```Python
temp_file.read()
```

När vi är klara med att använda vår temporära fil måste vi stänga den:

```Python
temp_file.close()
```

Det är också möjligt att behålla vår temporära fil efter att vårt program har avslutats genom att ange parametern `delete=False`:

```Python
temp_file = tempfile.NamedTemporaryFile(delete=False)
```

Det finns också andra alternativ för att skapa temporära filer, till exempel kan vi använda funktionen `TemporaryDirectory()` för att skapa en temporär mapp istället för en fil.

## Djupdykning

Modulen `tempfile` tillhandahåller flera användbara funktioner för att hantera temporära filer och mappar. Till exempel kan vi också använda funktionen `mkstemp()` för att skapa en temporär fil och få dess namn som återgår som en sträng. Detta kan vara användbart om vi behöver använda namnet på vår temporära fil i vår kod.

Det finns också andra parametrar som vi kan ange vid skapandet av vår temporära fil, som `suffix` för att ange en filändelse, `dir` för att ange en mapp där den temporära filen ska skapas, och `text` för att ange att filen ska öppnas i textläge istället för binärläge.

En annan viktig aspekt att tänka på när vi arbetar med temporära filer är att se till att vi stänger och tar bort dem när vi är klara. Om vi inte använder parametern `delete=False` kommer vår temporära fil att raderas automatiskt när vårt program avslutas. Men om vi behåller vår temporära fil måste vi se till att ta bort den manuellt för att undvika att den tar upp onödig diskutrymme.

## Se även

- [Python dokumentation om modulen `tempfile`](https://docs.python.org/sv/3/library/tempfile.html)
- [Real Python - Working with Temporary Files and Directories in Python](https://realpython.com/working-with-temporary-files-in-python/)