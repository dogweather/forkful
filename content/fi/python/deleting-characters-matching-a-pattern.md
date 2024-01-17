---
title:                "Kaavion mukaisten merkkien poistaminen"
html_title:           "Python: Kaavion mukaisten merkkien poistaminen"
simple_title:         "Kaavion mukaisten merkkien poistaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Poistan tekstin osia, jotka täsmäävät tiettyyn kaavaan, esimerkiksi tiettyyn merkkijonoon. Tätä tehdään yleensä tietyn ohjelman vaatimusten tai käyttötarkoituksen vuoksi.

## Miten:
Esimerkkikoodit ja tulosteet ```Python ... ``` koodilohkoissa.

```python
# Esimerkki #1:
teksti = "Tämä on esimerkki"
uusi_teksti = re.sub("e", "", teksti)
print(uusi_teksti)
# Output: Tämä on simerkki

# Esimerkki #2:
teksti = "12345"
uusi_teksti = re.sub("[0-9]", "", teksti)
print(uusi_teksti)
# Output: 

# Esimerkki #3:
teksti = "Hei kaikki!"
uusi_teksti = re.sub("[aeiou]", "", teksti)
print(uusi_teksti)
# Output: H kll!
```
## Syvemmälle:
Alun perin "poistaminen" merkkijonoista tehtiin manuaalisesti mekaanisilla kirjoituskoneilla. Nykyään käytetään säännöllisiä lausekkeita (regexp), jotka mahdollistavat tarkempaa ja tehokkaampaa merkkijonojen käsittelyä. Vaihtoehtoisia tapoja poistaa merkkejä ovat esimerkiksi tietueiden suodatus ja leikkauksen käyttö.

## Katso myös:
- [Pythonin re-kirjasto](https://docs.python.org/3/library/re.html)
- [Wikipedian sivu säännöllisistä lausekkeista](https://fi.wikipedia.org/wiki/S%C3%A4%C3%A4nn%C3%B6llinen_lauseke)