---
title:    "Python: Säännöllisten ilmaisujen käyttö"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Miksi käyttää säännöllisiä lausekkeita Python-ohjelmoinnissa?

Säännölliset lausekkeet ovat tehokas työkalu tekstien ja merkkijonojen käsittelyssä Python-ohjelmoinnissa. Ne mahdollistavat monimutkaisen tekstinhaun ja muokkauksen yhdellä kertaa, mikä säästää aikaa ja vaivaa. Ne ovat myös erottamaton osa monia ohjelmointiprojekteja, ja niiden ymmärtäminen voi olla hyödyllistä myös muiden ohjelmointikielten parissa.

## Kuinka käyttää säännöllisiä lausekkeita Pythonissa?

Säännölliset lausekkeet toimivat merkkijonon muodossa ilmaisuna, jota käytetään hakemaan ja muokkaamaan tekstiä halutulla tavalla. Niiden käyttöönotto Pythonissa vaatii `re`-moduulin tuomisen:

```Python
import re
```

Yksinkertaisin esimerkki säännöllisen lausekkeen käytöstä on tarkistaa, löytyykö merkkijonosta haluttu sana. Seuraava esimerkki tulostaa "Löytyi" jos sana "Python" löytyy merkkijonosta, muuten se tulostaa "Ei löytynyt":

```Python
merkkijono = "Opi käyttämään Pythonia"
if re.search("Python", merkkijono):
    print("Löytyi")
else:
    print("Ei löytynyt")
```

Säännöllisiä lausekkeita voi myös käyttää merkkijonon osien käsittelyyn. Seuraava esimerkki jakaa merkkijonon välilyönneistä ja tulostaa jokaisen sanan erikseen:

```Python
merkkijono = "Tämä on esimerkki"
sanat = re.split(" ", merkkijono)
print(sanat)
```

Tämä tulostaa: `['Tämä', 'on', 'esimerkki']`.

## Syvä sukellus säännöllisiin lausekkeisiin

Säännölliset lausekkeet voivat sisältää erilaisia erikoismerkkejä, jotka tekevät niiden käytöstä monipuolisempaa. Joitain yleisimpiä näistä merkeistä ovat:

- `.` - mikä tahansa merkki (paitsi rivinvaihto)
- `*` - edellistä merkkiä voi esiintyä nolla tai useampi kerta
- `+` - edellistä merkkiä voi esiintyä yksi tai useampi kerta
- `?` - edellistä merkkiä voi esiintyä nolla tai yksi kerta
- `()` - ryhmitys ja muuttujan tallennus

Esimerkiksi, jos haluamme etsiä kaikki Gmail-sähköpostiosoitteet, voimme käyttää seuraavaa säännöllistä lauseketta:

```Python
re.search(".+\@gmail\.com", "Sähköpostini on testi@gmail.com")
```

Tämä löytää ja palauttaa osoitteen "testi@gmail.com".

## Katso myös

- [Pythonin virallinen dokumentaatio säännöllisille lausekkeille](https://docs.python.org/3/howto/regex.html)
- [Regex101 - työkalu säännöllisten lausekkeiden testaamiseen](https://regex101.com/)