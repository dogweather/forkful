---
date: 2024-01-26 03:46:43.145043-07:00
description: "Kuinka: T\xE4ss\xE4 tietopaketti numeroiden py\xF6rist\xE4misest\xE4\
  \ Pythonissa."
lastmod: '2024-03-13T22:44:56.138616-06:00'
model: gpt-4-0125-preview
summary: "T\xE4ss\xE4 tietopaketti numeroiden py\xF6rist\xE4misest\xE4 Pythonissa."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Tässä tietopaketti numeroiden pyöristämisestä Pythonissa:

```python
# Pyöristä numero lähimpään kokonaislukuun
print(round(8.67))  # Tulostaa: 9

# Pyöristä numero määriteltyyn desimaalipaikkojen määrään
print(round(8.67, 1))  # Tulostaa: 8.7

# Parilliset numerot pyöristetään alas ja parittomat numerot ylös, kun ne ovat yhtä kaukana
print(round(2.5))  # Tulostaa: 2
print(round(3.5))  # Tulostaa: 4
```

## Syväsukellus
Pythonissa `round()` ei vain hylkää desimaaleja. Historiallisesti Python, kuten monet muut kielet, noudattaa "pyöristä puolikas parilliseen" tai "pankkipyöristystä". Tämä minimoi kumulatiivisen virheen summille tai keskiarvoille, mikä on tärkeää taloudellisissa laskelmissa.

Vaihtoehtoja varten sinulla on `math.floor()` ja `math.ceil()` Pythonin matematiikkamoduulista, jotka vetävät numeroita alaspäin tai ylöspäin seuraavaan kokonaislukuun. Mutta jos tavoittelet tarkkuutta, `decimal`-moduulin `quantize()` antaa sinun määrittää pyöristyskäytöksen.

Pinnan alla `round()` käsittelee binäärisiä liukulukulukuja. Koska jotkut desimaalit eivät voida ilmaista tarkasti binäärinä, saatat kohdata yllätyksiä, kuten `round(2.675, 2)` ei muutu odotetusti `2.68`:ksi. Tässä kohtaa `decimal` tai `fractions` tulevat avuksi korkean tarkkuuden saavuttamiseksi.

## Katso myös
- Pythonin dokumentaatio sisäänrakennetuista funktioista: https://docs.python.org/3/library/functions.html#round
- Desimaalin kiinteäpiste- ja liukuluku aritmetiikka: https://docs.python.org/3/library/decimal.html
- Pythonin matematiikkamoduuli: https://docs.python.org/3/library/math.html
