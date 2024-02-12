---
title:                "Kontrollera om en katalog existerar"
aliases: - /sv/python/checking-if-a-directory-exists.md
date:                  2024-02-03T19:08:12.733374-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns i Python handlar om att verifiera närvaron av en mapp i filsystemet innan man utför operationer som att läsa eller skriva filer. Programmerare gör detta för att undvika fel såsom `FileNotFoundError`, vilket säkerställer att applikationen beter sig pålitligt och inte kraschar när den försöker interagera med mappar.

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
