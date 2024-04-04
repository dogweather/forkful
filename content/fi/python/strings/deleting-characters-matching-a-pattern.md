---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Miten: .'
lastmod: '2024-04-04T01:27:58.624653-06:00'
model: gpt-4-0125-preview
summary: .
title: Mallia vastaavien merkkien poistaminen
weight: 5
---

## Miten:
```Python
import re

# Esimerkkimerkkijono
text = "Hello, World! 1234"

# Poista kaikki numerot
ei_numeroita = re.sub(r'\d', '', text)
print(ei_numeroita)  # Tuloste: "Hello, World! "

# Poista välimerkit
ei_valimerkkeja = re.sub(r'[^\w\s]', '', text)
print(ei_valimerkkeja)  # Tuloste: "Hello World 1234"

# Poista vokaalit
ei_vokaaleja = re.sub(r'[aeiouAEIOU]', '', text)
print(ei_vokaaleja)  # Tuloste: "Hll, Wrld! 1234"
```

### Itse kirjoittamani räätälöity funktio

Teen tätä tarpeeksi usein, että se on tiivistetty tähän `delete()` funktioon. Se on myös hyvä esittely [doctestien](https://docs.python.org/3/library/doctest.html) käytössä:

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```



## Syväsukellus
Merkkijonoista kuviota vastaavien merkkien poistokäytännöllä on syvät juuret tietojenkäsittelytieteessä, jäljittäen takaisin varhaisiin Unix-työkaluihin kuten `sed` ja `grep`. Pythonissa `re` moduuli tarjoaa tämän kyvyn, hyödyntäen säännöllisiä lausekkeita - tehokkaan ja monipuolisen työkalun tekstin käsittelyyn.

Vaihtoehtoja `re` moduulille sisältävät:
- Merkkijonon metodit kuten `replace()` yksinkertaisissa tapauksissa.
- Kolmannen osapuolen kirjastot kuten `regex` monimutkaisempiin kaavoihin ja parempaan Unicode-tukeen.

Kun käytät `re.sub()`, Python-tulkki kääntää kaavan joukoksi bytecodeja, joita tilakone prosessoi suorittaen kuviotunnistuksen suoraan syötetekstissä. Tämä toiminta voi olla resurssi-intensiivistä suurille merkkijonoille tai monimutkaisille kaavoille, joten suorituskyvyn huomioon ottaminen on ratkaisevaa suuren datan käsittelyssä.

## Katso myös
- [Python `re` moduulin dokumentaatio](https://docs.python.org/3/library/re.html): Viralliset dokumentit Pythonin säännöllisistä lausekkeista.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Kattava opas säännöllisistä lausekkeista.
- [Real Pythonin opastus regexistä](https://realpython.com/regex-python/): Säännöllisten lausekkeiden todellisen maailman sovellukset Pythonissa.
