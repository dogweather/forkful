---
title:                "Kirjoittaminen vakiovirheeseen"
date:                  2024-01-19
html_title:           "Bash: Kirjoittaminen vakiovirheeseen"
simple_title:         "Kirjoittaminen vakiovirheeseen"

category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä ja Miksi?)
Kirjoitetaan standardivirheeseen, kun halutaan erottaa ohjelman virheilmoitukset ja tavallinen output. Näin logien luku ja virheiden käsittely helpottuvat.

## How to: (Kuinka tehdä:)
```Python
import sys

# Tavallinen tuloste
print("Tämä menee standarditulosteeseen.")

# Virheilmoitus
print("Tämä on virheilmoitus!", file=sys.stderr)

# Esimerkkitulostus
# Tavallinen tuloste:
#   Tämä menee standarditulosteeseen.
# Virheilmoitus:
#   Tämä on virheilmoitus!
```

## Deep Dive (Syväsukellus)
Ennen oli vain tuloste. Unixissa keksittiin standardivirhe eroon ohjaamaan virheet. Voit käyttää `sys.stderr.write()`, joka on matalan tason vaihtoehto. Tämä kirjoittaa virheilmoitusten virtaan ilman puskurointia, toisin kuin `print`.

## See Also (Katso Myös)
- [Pythonin virallinen dokumentaatio sys-moduulista](https://docs.python.org/3/library/sys.html)
- [Unix-philosophy ja standard streams](http://www.catb.org/esr/writings/taoup/html/ch01s06.html)
- [SO-ketju standardivirheen käsittelystä](https://stackoverflow.com/questions/5574702/how-to-print-to-stderr-in-python)
