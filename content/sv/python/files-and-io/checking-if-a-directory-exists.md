---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:12.733374-07:00
description: "Hur g\xF6r man: Python tillhandah\xE5ller inbyggda s\xE4tt att kontrollera\
  \ om en mapp existerar genom att anv\xE4nda modulerna `os` och `pathlib`. H\xE4\
  r \xE4r exempel f\xF6r\u2026"
lastmod: '2024-03-13T22:44:37.497256-06:00'
model: gpt-4-0125-preview
summary: "Python tillhandah\xE5ller inbyggda s\xE4tt att kontrollera om en mapp existerar\
  \ genom att anv\xE4nda modulerna `os` och `pathlib`."
title: Kontrollera om en katalog existerar
weight: 20
---

## Hur gör man:
Python tillhandahåller inbyggda sätt att kontrollera om en mapp existerar genom att använda modulerna `os` och `pathlib`. Här är exempel för båda:

### Använda `os`-modulen
```python
import os

# Ange sökvägen till mappen
dir_path = "/path/to/directory"

# Kontrollera om mappen finns
if os.path.isdir(dir_path):
    print(f"Mappen {dir_path} finns.")
else:
    print(f"Mappen {dir_path} finns inte.")
```

### Använda `pathlib`-modulen
```python
from pathlib import Path

# Ange sökvägen till mappen
dir_path = Path("/path/to/directory")

# Kontrollera om mappen finns
if dir_path.is_dir():
    print(f"Mappen {dir_path} finns.")
else:
    print(f"Mappen {dir_path} finns inte.")
```

### Tredjepartsbibliotek
Även om Pythons standardbibliotek är tillräckligt för att kontrollera om en mapp finns, kan bibliotek som `pathlib2` vara alternativ för konsistens över Pythonversioner eller ytterligare funktionalitet.

***Obs:*** Med de senaste versionerna av Python är `pathlib` robust nog för de flesta användningsfall, vilket gör tredjepartsbibliotek mindre nödvändiga för denna specifika uppgift.
