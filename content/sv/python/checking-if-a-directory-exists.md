---
title:                "Python: Kontrollera om en mapp existerar"
simple_title:         "Kontrollera om en mapp existerar"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns kan vara en mycket användbar teknik inom Python-programmering. Det gör det möjligt att skapa dynamiska program som kan hantera olika filstrukturer och möjliggör också korrekt felhantering.

## Hur man gör det

Enklaste sättet att kontrollera om en mapp finns är att använda `os.path.exists()` funktionen. Detta är en inbyggd funktion i Python som tar in en sökväg som argument och returnerar `True` om mappen finns och `False` om den inte gör det.

```Python
import os

mapp = "/home/user/Dokument"

if os.path.exists(mapp):
    print("Mappen finns!")
else:
    print("Mappen finns inte.")
```
```python
Mappen finns!
```

Det finns också andra funktioner som `os.path.isdir()` och `os.path.isfile()` som gör det möjligt att kontrollera om en sökväg är en mapp eller fil. Dessa funktioner kan vara användbara om du vill utföra olika åtgärder baserat på typen av filstruktur som du arbetar med.

```Python
import os

sokvag = "/home/user/Bilder/profilbild.jpg"

if os.path.isfile(sokvag):
    print("Det här är en fil.")
elif os.path.isdir(sokvag):
    print("Det här är en mapp.")
else:
    print("Det här är inte en fil eller mapp.")
```
```python
Det här är en fil.
```

## Djupdykning

Förutom de olika inbyggda funktionerna som används för att kontrollera om en mapp finns, finns det också olika bibliotek som kan användas för att uppnå samma mål. Ett sådant bibliotek är `pathlib` som gör det möjligt att arbeta med sökvägar på ett objektorienterat sätt.

```Python
from pathlib import Path

mapp = Path("/home/user")

if mapp.exists():
    print("Mappen finns!")
else:
    print("Mappen finns inte.")
```
```python
Mappen finns!
```

## Se också

- [Python os.path dokumentation](https://docs.python.org/3/library/os.path.html)
- [Python pathlib dokumentation](https://docs.python.org/3/library/pathlib.html)