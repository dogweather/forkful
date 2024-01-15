---
title:                "Tarkistetaan löytyykö hakemistoa"
html_title:           "Python: Tarkistetaan löytyykö hakemistoa"
simple_title:         "Tarkistetaan löytyykö hakemistoa"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

Voit joutua tarkistamaan, onko hakemisto olemassa, jos haluat varmistaa, että tiettyä tiedostopolkua voidaan käyttää, ennen kuin suoritat tietyt koodirivit.

## Kuinka

```python
# Tarkista, onko hakemisto olemassa
import os

hakemisto = "/Users/kayttaja/Documents"
if os.path.exists(hakemisto):
  print("Hakemisto on olemassa.")
else: 
  print("Hakemistoa ei ole olemassa.")
```

Tämä yksinkertainen esimerkkikoodi käyttää Pythonin "os" -moduulia tarkistaakseen, onko hakemisto olemassa. Jos hakemisto on olemassa, koodi tulostaa viestin, joka ilmoittaa siitä. Muussa tapauksessa tulostetaan toinen viesti.

## Syvällinen perehtyminen

Tarkistaessamme, onko hakemisto olemassa, käytämme "os.path.exists" -funktiota, joka palauttaa True tai False, riippuen siitä, löytyykö määritetty polku. Tällä tavoin voimme ensin tarkistaa hakemiston olemassaolon ennen kuin käytämme sitä esimerkiksi tiedostojen avaamiseen tai tallentamiseen.

## Katso myös

- Pythonin "os" -moduuli: https://docs.python.org/3/library/os.html
- Tiedostopolun tarkistamisesta kohti hakemistoa: https://www.journaldev.com/20562/python-check-if-file-directory-exists