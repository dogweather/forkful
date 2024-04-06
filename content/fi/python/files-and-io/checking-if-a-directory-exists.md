---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:15.888586-07:00
description: "Miten: Python tarjoaa natiiveja tapoja tarkistaa hakemiston olemassaolo\
  \ k\xE4ytt\xE4en `os` ja `pathlib` -moduuleita. T\xE4ss\xE4 ovat esimerkit molemmille."
lastmod: '2024-03-13T22:44:56.157932-06:00'
model: gpt-4-0125-preview
summary: "Python tarjoaa natiiveja tapoja tarkistaa hakemiston olemassaolo k\xE4ytt\xE4\
  en `os` ja `pathlib` -moduuleita."
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
