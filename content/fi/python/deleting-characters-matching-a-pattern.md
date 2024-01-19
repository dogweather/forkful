---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Kuvioita vastaavien merkkien poistaminen Pythonilla: Mikä, Miksi ja Miten?

## Mikä & Miksi?

Kuvionsopivien merkkien poistaminen on tapa poistaa kaikki merkit, jotka täyttävät tietyn mallin. Ohjelmoijat tekevät tämän yksinkertaistamaan tai jalostamaan dataa, tai ehkä panemaan sen formaattiin, jonka muu koodi voi käsitellä.

## Näin tehdään:

Pythonissa `str.replace()` funktio tai `re.sub()` funktio ovat hyviä työkaluja tähän tarkoitukseen. Tässä on joitain esimerkkejä:
```python
# str.replace() -esimerkki
s = 'Hei, minun nimeni on Python!'
s = s.replace('!', '.')
print(s)  # Tulostaa: 'Hei, minun nimeni on Python.'

# re.sub() -esimerkki
import re
s = 'Hei, minun nimeni on Python!'
s = re.sub('[!,]', '.', s)
print(s)  # Tulostaa: 'Hei. minun nimeni on Python.'
```

## Syvällisempi tieto:

Kuvionsopivien merkkien poistaminen tunnetaan myös regex-sanalla ja se on ollut ohjelmoinnin kulmakivi vuodesta 1951, kun se otettiin käyttöön ensimmäistä kertaa.

Vaihtoehtoisia metodeja ovat `str.translate()` ja `str.maketrans()`, mutta ne ovat hieman monimutkaisempia ja hitaampia, koska ne luovat aluksi taulukon.

Kun koodi poistaa merkkejä, se käy läpi jokaisen merkin merkkijonossa ja tarkistaa, täyttääkö se annetun mallin. Jos se on täyttänyt, merkki poistetaan.

## Lisää tietoa:

Suosittelen lukemaan seuraavat artikkelit lisätietojen saamiseksi asiasta:

1. [Pythonin virallinen dokumentaatio str.replace() -funktiosta](https://docs.python.org/3/library/stdtypes.html#str.replace)
2. [Pythonin virallinen dokumentaatio re.sub() -funktiosta](https://docs.python.org/3/library/re.html#re.sub)
3. [Pythonin virallinen dokumentaatio str.translate() ja str.maketrans() -funktioista](https://docs.python.org/3/library/stdtypes.html#str.translate)
4. [Lisätietoja regex-patternista](https://en.wikipedia.org/wiki/Regular_expression)