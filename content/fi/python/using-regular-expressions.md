---
title:    "Python: Säännöllisten lausekkeiden käyttö"
keywords: ["Python"]
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita?

Säännöllisten lausekkeiden käyttäminen ohjelmoinnissa voi säästää aikaa ja vaivaa monimutkaisten tekstien käsittelyssä. Voit esimerkiksi helposti etsiä ja korvata tietyt merkkijonot tai tunnistaa tiettyjä kaavoja tekstistä.

## Kuinka käyttää säännöllisiä lausekkeita

Säännöllisten lausekkeiden käyttö Pythonissa on helppoa. Alkuun pääsemiseksi tarvitset vain "re" -moduulin, joka sisältää tarvittavat toiminnot. Seuraavassa on muutama esimerkki säännöllisten lausekkeiden käytöstä erilaisten tekstien käsittelyyn:

### Etsi ja tulosta tiettyä merkkijonoa sisältävät rivit

```Python
import re

teksti = "Tänään on kaunis päivä, mutta huomenna sataa."

tulokset = re.findall("päivä", teksti)

print(tulokset) # ["päivä"]
```

### Vaihda merkkijonon osa toiseen

```Python
import re

teksti = "Tänään on kaunis päivä, mutta huomenna sataa."

uusi_teksti = re.sub("päivä", "aamu", teksti)

print(uusi_teksti) # Tänään on kaunis aamu, mutta huomenna sataa.
```

### Tunnista päivämäärä

```Python
import re

teksti = "Tänään on 5. maaliskuuta 2021."

tulokset = re.search("\d+\. \w+ \d+", teksti)

print(tulokset.group()) # 5. maaliskuuta 2021
```

## Syvällinen tarkastelu säännöllisistä lausekkeista

Vaikka säännöllisten lausekkeiden peruskäyttö on helppo oppia, niiden täydellinen hallitseminen vaatii paljon harjoitusta ja perehtymistä eri sääntöihin ja ilmauksiin. Esimerkiksi säännöllisillä lausekkeilla voit määrittää tarkemmin mitä merkkejä tai kaavoja haluat etsiä tai korvata. Voit myös käyttää erilaisia operaattoreita, kuten "|", "( )" ja "+" saadaksesi tarkempia tuloksia.

## Katso myös

- [Pythonin säännölliset lausekkeet (re) -dokumentaatio](https://docs.python.org/3/library/re.html)
- [Regular Expressions Cheat Sheet](https://www.debuggex.com/cheatsheet/regex/python)
- [Regexone: interaktiivinen oppimistyökalu säännöllisille lausekkeille](https://regexone.com/)