---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: 'Kuinka: .'
lastmod: '2024-04-04T02:02:42.244023-06:00'
model: gpt-4-0125-preview
summary: .
title: Kaavan mukaisten merkkien poistaminen
weight: 5
---

## Kuinka:
```Python
import re

# Esimerkkimerkkijono
teksti = "Hello, World! 1234"

# Poistetaan kaikki numerot
ei_numeroita = re.sub(r'\d', '', teksti)
print(ei_numeroita)  # Tuloste: "Hello, World! "

# Poistetaan välimerkit
ei_valimerkkeja = re.sub(r'[^\w\s]', '', teksti)
print(ei_valimerkkeja)  # Tuloste: "Hello World 1234"

# Poistetaan vokaalit
ei_vokaaleja = re.sub(r'[aeiouAEIOU]', '', teksti)
print(ei_vokaaleja)  # Tuloste: "Hll, Wrld! 1234"
```

### Oma räätälöity funktioni

Teen tätä tarpeeksi usein, joten muodostin yksinkertaisen `delete()` funktion. Se on myös hyvä esimerkki [doctesteista](https://docs.python.org/3/library/doctest.html):

```python
def delete(merkkijono: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", merkkijono)
```



## Syväsukellus
Merkkijonosta kuviota vastaavien merkkien poistamisen käytäntö juontaa juurensa syvälle tietojenkäsittelytieteeseen, aina varhaisiin Unix-työkaluihin kuten `sed` ja `grep`. Pythonissa `re`-moduuli tarjoaa tämän kyvyn, hyödyntäen säännöllisiä lausekkeita – tehokkaan ja monipuolisen työkalun tekstinkäsittelyyn.

Vaihtoehtoja `re`-moduulille sisältävät:
- Merkkijonomenetelmät kuten `replace()` yksinkertaisiin tapauksiin.
- Kolmannen osapuolen kirjastot kuten `regex` monimutkaisempiin kaavoihin ja parempaan Unicode-tukeen.

Kun käytät `re.sub()`, Python-tulkki kääntää kaavan joukoksi tavukoodeja, joita tilakone käsittelee suorittaen kuviotäsmäyksen suoraan syötetekstissä. Tämä operaatio voi olla resurssi-intensiivistä suurille merkkijonoille tai monimutkaisille kaavoille, joten suorituskyvyn huomioiminen on tärkeää suurten datamäärien käsittelyssä.

## Katso myös
- [Python `re`-moduulin dokumentaatio](https://docs.python.org/3/library/re.html): Viralliset dokumentit Pythonin säännöllisistä lausekkeista.
- [Regular-Expressions.info](https://www.regular-expressions.info/): Kattava opas säännöllisiin lausekkeisiin.
- [Real Python -opastus regexistä](https://realpython.com/regex-python/): Todellisen maailman sovelluksia säännöllisistä lausekkeista Pythonissa.
