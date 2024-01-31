---
title:                "Onko hakemisto olemassa? Tarkistaminen"
date:                  2024-01-20T14:58:06.040457-07:00
simple_title:         "Onko hakemisto olemassa? Tarkistaminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Tarkistetaan, onko kansio olemassa ennen tiedostojen lukemista tai kirjoittamista. Tämä estää virheet ja tekee ohjelmasta vakaamman.

## How to: (Kuinka tehdä:)
```Python
import os

# Tarkistetaan, löytyykö kansio
def kansio_olemassa(polku):
    return os.path.isdir(polku)

# Esimerkkikäyttö
polku = "/esimerkki/kansio"
if kansio_olemassa(polku):
    print("Kansio löytyy!")
else:
    print("Kansiota ei ole olemassa.")

# Tulostus esimerkki
# Kansio löytyy! TAI Kansiota ei ole olemassa.
```

## Deep Dive (Syväsukellus)
Vanhemmissa Python-versioissa `os.path.isdir()` oli yleinen tapa tarkistaa kansion olemassaolo. Python 3.4:n jälkeen voimme käyttää `pathlib`-moduulia, joka tarjoaa objektilähtöisen tavan käsitellä tiedostopolkuja. Se on usein selkeämpi ja mukautuu hyvin erilaisiin tilanteisiin.

Vaihtoehtoisesti:
```Python
from pathlib import Path

# Tarkistetaan, löytyykö kansio
polku = Path("/esimerkki/kansio")
if polku.is_dir():
    print("Kansio löytyy!")
else:
    print("Kansiota ei ole olemassa.")
```

Implementoinnissa kannattaa ottaa huomioon käyttöoikeudet. Jos ohjelmalla ei ole riittäviä oikeuksia lukea kansiota, voimme saada virheellisen `False`, vaikka kansio olisikin olemassa.

## See Also (Lisätietoja)
- Python `os` module documentation: https://docs.python.org/3/library/os.html
- Python `pathlib` module documentation: https://docs.python.org/3/library/pathlib.html
- Käyttöoikeudet ja niiden hallinta Pythonissa: https://docs.python.org/3/library/os.html#os.access
