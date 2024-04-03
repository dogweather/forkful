---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:56:32.099035-07:00
description: 'Hoe: Python maakt het controleren op een directory eenvoudig met de
  `os` en `pathlib` modules: Gebruikmakend van `os.path`.'
lastmod: '2024-03-13T22:44:50.390915-06:00'
model: gpt-4-0125-preview
summary: Python maakt het controleren op een directory eenvoudig met de `os` en `pathlib`
  modules.
title: Controleren of een directory bestaat
weight: 20
---

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
