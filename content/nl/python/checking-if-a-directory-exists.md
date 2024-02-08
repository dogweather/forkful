---
title:                "Controleren of een directory bestaat"
aliases:
- nl/python/checking-if-a-directory-exists.md
date:                  2024-01-28T21:56:32.099035-07:00
model:                 gpt-4-0125-preview
simple_title:         "Controleren of een directory bestaat"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Controleren of een directory bestaat in Python gaat over het bevestigen van de aanwezigheid van een map op het bestandssysteem voordat er acties op worden uitgevoerd. Programmeurs doen dit om fouten zoals het proberen te openen of schrijven naar een directory die er niet is, te vermijden.

## Hoe:

Python maakt het controleren op een directory eenvoudig met de `os` en `pathlib` modules:

Gebruikmakend van `os.path`:
```python
import os

# Controleren of de directory bestaat
if os.path.isdir("/pad/naar/directory"):
    print("De directory bestaat.")
else:
    print("De directory bestaat niet.")
```

Gebruikmakend van `pathlib`:
```python
from pathlib import Path

# Controleren of de directory bestaat
directory = Path("/pad/naar/directory")
if directory.is_dir():
    print("De directory bestaat.")
else:
    print("De directory bestaat niet.")
```

Voorbeelduitvoer:
```
De directory bestaat.
```
of
```
De directory bestaat niet.
```

## Diepgaand:

Historisch gezien gebruikte Python de `os` module voor bestandssysteemoperaties. Echter, `os.path.isdir()` was de feitelijke standaard voor het controleren van directories. Het probleem was dat `os.path` werkte met strings voor paden, wat onhandig kon zijn.

Enter de modernere `pathlib` module, geïntroduceerd in Python 3.4. Het gebruikt object-georiënteerde paden, wat de code leesbaarder en bondiger maakt. Nu heb je `Path.is_dir()`, een methode die niet alleen je code netter maakt, maar er is ook iets aangenaams aan het aaneenkoppelen van methoderoepen aan een Path-object.

Als deze methoden `False` teruggeven voor een niet-bestaande directory, kan dat twee dingen betekenen: of de directory is er echt niet, of je programma heeft niet de toestemming om het te zien.

## Zie ook:

1. Documentatie van de `os` module: https://docs.python.org/3/library/os.html
2. Documentatie van de `pathlib` module: https://docs.python.org/3/library/pathlib.html
3. Bestandssysteempermissies in Python: https://docs.python.org/3/library/os.html#os.access
