---
date: 2024-01-20 17:39:18.249088-07:00
description: "How to: - Miten: Merkkijonojen muuntaminen pieniksi kirjaimiksi on ollut\
  \ osa ohjelmointikieli\xE4 jo pitk\xE4\xE4n. Algoritmi itsess\xE4\xE4n on yksinkertainen:\
  \ jokainen\u2026"
lastmod: '2024-04-05T22:51:10.291859-06:00'
model: gpt-4-1106-preview
summary: "- Miten: Merkkijonojen muuntaminen pieniksi kirjaimiksi on ollut osa ohjelmointikieli\xE4\
  \ jo pitk\xE4\xE4n. Algoritmi itsess\xE4\xE4n on yksinkertainen: jokainen suuri\
  \ kirjain korvataan vastaavalla pienell\xE4 kirjaimella. Pythonin `lower()`-metodi\
  \ tekee t\xE4m\xE4n kaikille merkeille, joilla on pieni kirjain vastine Unicode-standardissa.\
  \ Vaihtoehtoina on k\xE4ytt\xE4\xE4 `casefold()`-metodia, joka on aggressiivisempi\
  \ ja ottaa huomioon enemm\xE4n kielellisi\xE4 erityistapauksia, kuten saksan \xDF\
  -kirjaimen. Toteutusyksityiskohtia tutkiessa on hyv\xE4 pit\xE4\xE4 mieless\xE4\
  , ett\xE4 joissakin kieliss\xE4 muunnokset pieniksi kirjaimiksi eiv\xE4t ole niin\
  \ suoraviivaisia ja voivat vaatia erityist\xE4 k\xE4sittely\xE4 Unicode-normalisointien\
  \ my\xF6t\xE4."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

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
