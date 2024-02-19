---
aliases:
- /fi/python/converting-a-string-to-lower-case/
date: 2024-01-20 17:39:18.249088-07:00
description: "Muuttamalla merkkijonon pieniksi kirjaimiksi varmistetaan, ett\xE4 teksti\
  \ on yhdenmukainen ja vertailukelpoinen. T\xE4m\xE4 on hy\xF6dyllist\xE4 erityisesti\
  \ tietojen\u2026"
lastmod: 2024-02-18 23:09:07.170003
model: gpt-4-1106-preview
summary: "Muuttamalla merkkijonon pieniksi kirjaimiksi varmistetaan, ett\xE4 teksti\
  \ on yhdenmukainen ja vertailukelpoinen. T\xE4m\xE4 on hy\xF6dyllist\xE4 erityisesti\
  \ tietojen\u2026"
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Muuttamalla merkkijonon pieniksi kirjaimiksi varmistetaan, että teksti on yhdenmukainen ja vertailukelpoinen. Tämä on hyödyllistä erityisesti tietojen syöttämisen ja hakutoimintojen yhteydessä.

## How to: - Miten:
```Python
# Pythonilla merkkijonon muuttaminen pieniksi kirjaimiksi on helppoa:
teksti = "Hei Maailma!"
pienet_kirjaimet = teksti.lower()

print(pienet_kirjaimet)
```

Tulostuu:
```
hei maailma!
```

## Deep Dive - Syväsukellus:
Merkkijonojen muuntaminen pieniksi kirjaimiksi on ollut osa ohjelmointikieliä jo pitkään. Algoritmi itsessään on yksinkertainen: jokainen suuri kirjain korvataan vastaavalla pienellä kirjaimella. Pythonin `lower()`-metodi tekee tämän kaikille merkeille, joilla on pieni kirjain vastine Unicode-standardissa.

Vaihtoehtoina on käyttää `casefold()`-metodia, joka on aggressiivisempi ja ottaa huomioon enemmän kielellisiä erityistapauksia, kuten saksan ß-kirjaimen.

Toteutusyksityiskohtia tutkiessa on hyvä pitää mielessä, että joissakin kielissä muunnokset pieniksi kirjaimiksi eivät ole niin suoraviivaisia ja voivat vaatia erityistä käsittelyä Unicode-normalisointien myötä.

## See Also - Katso Myös:
- Pythonin virallinen dokumentaatio `lower()`-metodista: https://docs.python.org/3/library/stdtypes.html#str.lower
- Tietoa Unicode-merkistöstä ja normalisoinnista: https://unicode.org/reports/tr15/
- `casefold()`-metodin dokumentaatio: https://docs.python.org/3/library/stdtypes.html#str.casefold
