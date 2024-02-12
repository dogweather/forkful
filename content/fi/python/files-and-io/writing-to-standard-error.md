---
title:                "Kirjoittaminen standardivirheeseen"
aliases:
- /fi/python/writing-to-standard-error.md
date:                  2024-02-03T19:34:29.743769-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kirjoittaminen standardivirheeseen"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/writing-to-standard-error.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?
Kirjoittaminen standardivirheeseen Pythonilla tarkoittaa ohjelmasi virheilmoitusten tai diagnostiikkatietojen suuntaamista virrasta (`stderr`), erilleen tavallisesta tulosteesta (`stdout`). Ohjelmoijat tekevät näin erotellakseen normaalit ohjelman tulosteet virheviesteistä, mikä helpottaa vianetsintää ja lokianalyysia.

## Kuinka:
### Käyttäen `sys.stderr`
Pythonin sisäänrakennettu `sys`-moduuli mahdollistaa eksplisiittisen kirjoittamisen `stderr`-virtaan. Tämä lähestymistapa on suoraviivainen yksinkertaisia virheviestejä tai diagnostiikkaa varten.

```python
import sys

sys.stderr.write('Virhe: Jotain meni pieleen.\n')
```
Näytetuloste (stderriin):
```
Virhe: Jotain meni pieleen.
```

### Käyttäen `print`-funktiota
Pythonin `print`-funktio voi uudelleenohjata tulosteensa `stderr`-virtaan määrittelemällä `file`-parametrin. Tämä menetelmä on hyödyllinen, kun halutaan hyödyntää `print`in käyttäjäystävällisyyttä käsiteltäessä virheviestejä.
```python
from sys import stderr

print('Virhe: Moduulin vika.', file=stderr)
```
Näytetuloste (stderriin):
```
Virhe: Moduulin vika.
```

### Käyttäen `logging`-moduulia
Kattavampaa ratkaisua varten Pythonin `logging`-moduuli voi ohjata viestejä `stderr`-virtaan ja paljon muuhunkin, kuten kirjoittaa tiedostoon tai mukauttaa viestin muotoa. Tämä menetelmä on parhaiten soveltuva sovelluksiin, jotka vaativat eri tasoja lokitusta, viestien muotoilua tai kohdepaikkoja.
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Virhe: Tietokantayhteys epäonnistui.')
```
Näytetuloste (stderriin):
```
ERROR:__main__:Virhe: Tietokantayhteys epäonnistui.
```

### Kolmannen osapuolen kirjastot: `loguru`
`loguru` on suosittu kolmannen osapuolen kirjasto, joka yksinkertaistaa lokitusta Python-sovelluksissa. Se ohjaa automaattisesti virheet `stderr`-virtaan, muiden ominaisuuksien joukossa.

Käyttääksesi `loguru`a, asenna se ensin pip:n kautta:
```shell
pip install loguru
```

Sitten sisällytä se Python-skriptiisi seuraavasti:
```python
from loguru import logger

logger.error('Virhe: Tiedoston avaaminen epäonnistui.')
```
Näytetuloste (stderriin):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Virhe: Tiedoston avaaminen epäonnistui.
```
