---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.888586-07:00
description: "Hakemiston olemassaolon tarkistaminen Pythonissa merkitsee kansion l\xE4\
  sn\xE4olon varmistamista tiedostoj\xE4rjestelm\xE4ss\xE4 ennen sellaisia toimintoja\
  \ kuin\u2026"
lastmod: '2024-03-13T22:44:56.157932-06:00'
model: gpt-4-0125-preview
summary: "Hakemiston olemassaolon tarkistaminen Pythonissa merkitsee kansion l\xE4\
  sn\xE4olon varmistamista tiedostoj\xE4rjestelm\xE4ss\xE4 ennen sellaisia toimintoja\
  \ kuin tiedostojen lukeminen tai kirjoittaminen."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Miten:
Python tarjoaa natiiveja tapoja tarkistaa hakemiston olemassaolo käyttäen `os` ja `pathlib` -moduuleita. Tässä ovat esimerkit molemmille:

### Käyttäen `os` -moduulia
```python
import os

# Määritä hakemiston polku
dir_path = "/path/to/directory"

# Tarkista, onko hakemisto olemassa
if os.path.isdir(dir_path):
    print(f"Hakemisto {dir_path} on olemassa.")
else:
    print(f"Hakemisto {dir_path} ei ole olemassa.")
```

### Käyttäen `pathlib` -moduulia
```python
from pathlib import Path

# Määritä hakemiston polku
dir_path = Path("/path/to/directory")

# Tarkista, onko hakemisto olemassa
if dir_path.is_dir():
    print(f"Hakemisto {dir_path} on olemassa.")
else:
    print(f"Hakemisto {dir_path} ei ole olemassa.")
```

### Kolmannen osapuolen kirjastot
Vaikkakin Pythonin vakio kirjasto riittää hakemiston olemassaolon tarkistamiseen, kirjastot kuten `pathlib2` voivat olla vaihtoehtoja yhtenäisyyden säilyttämiseksi Python-versioiden välillä tai lisätoiminnallisuuksien saamiseksi.

***Huom:*** Viimeisimpien Python-versioiden myötä `pathlib` on tarpeeksi vankka useimpiin käyttötapauksiin, mikä tekee kolmannen osapuolen kirjastoista vähemmän välttämättömiä tähän tiettyyn tehtävään.
