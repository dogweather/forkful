---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:58:05.742962-07:00
html_title:           "Fish Shell: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kolla om en katalog finns är att se till att en specifik mapp är där vi förväntar oss den. Programmerare gör detta för att undvika fel, när de till exempel ska läsa data från eller skriva data till filsystemet.

## Hur man gör:
För att kolla om en katalog finns i Python använder vi `os.path.exists()` eller `os.path.isdir()` från `os`-modulen. Nedan finns exempelkod:

```python
import os

# Kolla om en katalog finns
directory = '/var/www/html'

# Metod 1: Kolla om en sökväg existerar
exists = os.path.exists(directory)
print(f"Existerar '{directory}'? {exists}")

# Metod 2: Kolla om sökvägen är en katalog
is_dir = os.path.isdir(directory)
print(f"Är '{directory}' en katalog? {is_dir}")
```

Förväntad utskrift kan vara:

```
Existerar '/var/www/html'? True
Är '/var/www/html' en katalog? True
```

## Djupdykning
Funktionerna `os.path.exists()` och `os.path.isdir()` har funnits sedan tidiga versioner av Python och är den föredragna metoden för att hantera filsystemet. Även om de gör liknande saker, `os.path.isdir()` är striktare då den även kontrollerar om sökvägen är en katalog och inte bara en fil. Sidan `pathlib`, introducerad i Python 3.4, tillhandahåller ett objektorienterat gränssnitt för filsystemoperationer. Med `pathlib` kan vi använda `Path.exists()` och `Path.is_dir()` för samma ändamål med en modernare syntax.

Här är ett exempel med `pathlib`:

```python
from pathlib import Path

# Kolla om en katalog finns med pathlib
directory = Path('/var/www/html')

# Kolla om sökvägen existerar
exists = directory.exists()
print(f"Existerar '{directory}'? {exists}")

# Kolla om sökvägen är en katalog
is_dir = directory.is_dir()
print(f"Är '{directory}' en katalog? {is_dir}")
```

Observera att det är viktigt att hantera situationer där katalogen inte finns för att undvika `FileNotFoundError` när du försöker läsa från eller skriva till filsystemet.

## Se även
- [os.path documentation](https://docs.python.org/3/library/os.path.html)
- [pathlib documentation](https://docs.python.org/3/library/pathlib.html)
- [Filesystem error handling in Python](https://docs.python.org/3/library/exceptions.html#FileNotFoundError)
