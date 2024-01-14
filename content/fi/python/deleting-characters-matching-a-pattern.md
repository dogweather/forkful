---
title:    "Python: Merkkien poistaminen kaavan mukaisesti"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmoinnissa on tarpeen poistaa merkkejä, jotka vastaavat tiettyä kuviota. Tämä voi olla hyödyllistä esimerkiksi tekstikäsittelyssä tai datan käsittelyssä.

## Kuinka tehdä

Poistaaksesi merkkejä, jotka vastaavat tiettyä kuviota, voit käyttää Pythonin `re`-moduulia. Se mahdollistaa säännöllisten lausekkeiden käytön merkkijonojen käsittelyssä.

```Python
import re

teksti = "Tämä on esimerkkiteksti, jonka haluat muokata."
uusi_teksti = re.sub(r'e', '', teksti)

print(uusi_teksti)

# Output:
# Tämä on smrkksityksii, jonka haluat muokata.
```

Kuten näet, `re.sub()`-funktio poistaa kaikki merkit, jotka vastaavat ensimmäistä parametria, tässä tapauksessa kirjainta "e". Voit muokata tätä parametria vastaamaan haluamaasi kuviota.

## Syvempi sukellus

Mikäli haluat ymmärtää tarkemmin, miten säännölliset lausekkeet toimivat ja mitä muita mahdollisuuksia `re`-moduulilla on tarjota, voit tutustua sen [dokumentaatioon](https://docs.python.org/3/library/re.html). Sieltä löydät myös lisätietoa erilaisista parametreista ja vaihtoehdoista, joita voit käyttää `re.sub()`-funktiossa.

## Katso myös

- [Dokumentaatio: re-moduuli](https://docs.python.org/3/library/re.html)
- [Säännöllisten lausekkeiden opas](https://www.regular-expressions.info/)
- [Python Tutoriaali: Merkkijonojen käsittely](https://www.python.org/dev/peps/pep-0272/)