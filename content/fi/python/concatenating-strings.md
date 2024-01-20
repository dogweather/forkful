---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringien yhdistäminen tarkoittaa kahta tai useampaa merkkijonoa liitetään yhteen. Ohjelmoijat tekevät tämän, koska se mahdollistaa dynaamiset tekstit ja tiedon muokkaamisen.

## Näin teet:
Pythonissa on useita tapoja yhdistää merkkijonot. Kaksi yleisintä tapaa ovat `+` -operaattorin ja `join()` -funktion käyttäminen.

```python
# Tapa 1: käyttämällä '+' operaattoria
merkkijono1 = "Hei"
merkkijono2 = " maailma"
yhdistetty = merkkijono1 + merkkijono2
print(yhdistetty)  # Tulostaa: Hei maailma

# Tapa 2: käyttämällä 'join()' funktiota
sanat = ["Hei", "maailma"]
yhdistetty = " ".join(sanat)
print(yhdistetty)  # Tulostaa: Hei maailma
```

## Syvempi sukellus
Merkkijonojen yhdistämistä on käytetty ohjelmoinnissa sen alkuvaiheista lähtien. Pythonissa on useita tapoja yhdistää merkkijonoja, kuten `+` -operaattorin, `join()` -funktion, ja f-merkkijonot.

`+` -operaattori on yksinkertaisin tapa yhdistää merkkijonoja, mutta se luo uuden merkkijonon jokaisella yhdistämisoperaatiolla, mikä ei ole tehokasta suurille tietomäärille. 

`join()` -funktio on tehokkaampi, koska se luo uuden merkkijonon vain kerran. 

Python 3.6:ssa esiteltiin f-merkkijonot, jotka ovat erityisen käteviä merkkijonojen muotoilussa.

```python
nimi = "Pekka"
tervehdys = f"Hei, {nimi}"
print(tervehdys)  # Tulostaa: Hei, Pekka
```

## Katso myös
[Pythonin merkkijonojen yhdistäminen](https://docs.python.org/3/library/stdtypes.html#str.join)
[Pythonin f-merkkijonot](https://pep498.python.org/)
[Pythonin tekijänoikeuskäytäntö](https://docs.python.org/3/tutorial/inputoutput.html#fancier-output-formatting)