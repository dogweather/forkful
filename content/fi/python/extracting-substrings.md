---
title:    "Python: Alimerkkijonojen erottelu"
keywords: ["Python"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Monet Python-ohjelmoijat etsivät tapoja työskennellä merkkijonojen kanssa. Yksi tärkeä työkalu tätä varten on mahdollisuus poimia alimerkkijonoja tai pieniä osia merkkijonoista. Tässä blogikirjoituksessa opit syvemmin siitä, miksi ja kuinka poimia alimerkkijonoja Pythonissa.

## Kuinka

<img src="https://i.imgur.com/7d9tOnv.png" width="200" align="right">
Poimi alimerkkijonoja Pythonin `substring` -toiminnolla. Alla on esimerkki **koodiblokeineen**:

```Python
# Luo merkkijono
merkkijono = "Tämä on esimerkki merkkijonosta"

# Poimi tarvittava alimerkkijono
alimerkkijono = merkkijono[8:15]

# Tulosta alimerkkijono
print(alimerkkijono)

```
**Tulos**: "esimerk"

Voit myös määrittää alueen jättämättä toisen luvun tyhjäksi, jolloin alimerkkijono loppuu merkkijonon loppuun asti. Esimerkiksi: `merkkijono[8:]` palauttaa "esimerkijonosta".

## Syvempi sukellus

Miksi alimerkkijonojen poimiminen on tärkeää? Se antaa meille mahdollisuuden manipuloida ja käsitellä merkkijonoja paremmin. Se on erityisen hyödyllinen, kun työskentelemme suurten datamäärien kanssa ja haluamme etsiä tiettyjä sanoja tai lauseita merkkijonosta.

Voit myös käyttää muita sarakemerkintöjä alimerkkijonon poimimiseen. Esimerkiksi negatiiviset luvut aloittavat laskemisen merkkijonon lopusta päin. Joten `merkkijono[-7:]` antaisi saman tuloksen kuin `merkkijono[24:]` (sama sana "merkkijonosta").

Voit myös käyttää askelväliä määrittääksesi, kuinka monta merkkiä hyppäät jokaisen poimitun merkin välillä. Esimerkiksi `merkkijono[::2]` palauttaa jokaisen toisen merkin merkkijonosta ("Tä + n" + "ni napr + "mnkno" + "cta ämikjs" + "'stä").

## Katso myös

- [Pythonin merkkijonojen manipulointi](https://www.w3schools.com/python/python_strings.asp)
- [Pätkäjonojen poimiminen Pythonissa](https://www.geeksforgeeks.org/python-get-a-list-of-slice-from-given-string/)
- [Pythonin virallinen dokumentaatio](https://docs.python.org/3/library/stdtypes.html#string-methods) koskien merkkijonotoimintoja.