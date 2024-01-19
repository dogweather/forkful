---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

# Alistringien Poiminta Pythonilla

## Mikä & Miksi?
Alistringien poiminta on prosessi, jossa poimitaan osa muuttujasta tai tietotyypistä (tässä tapauksessa merkkijonosta) Python-ohjelmoinnissa. Ohjelmoijat tekevät sen tyypillisesti tiedon suodatukseen, analysointiin tai manipulointiin.

## Miten:

Pythonissa alistringien poiminta on suoraviivaista. Se voidaan tehdä käyttämällä merkkijonojen (str) leikkausoperaatiota.

```python
str = "Ohjelmointi on hauskaa"
# Poimi alistringi, joka alkaa indeksistä 0 ja päättyy indeksiin 10
print(str[0:11])
```

Tulosteeksi saadaan 'Ohjelmointi', koska se on osa alkuperäistä merkkijonoa, joka alkaa indeksistä 0 ja päättyy indeksiin 10.

## Syvempi Sukellus:

Alistringin poiminta Pythonilla perustuu leikkausoperaatioon, joka on historiallisesti periytynyt alkuperäisestä ohjelmointikieli ALGOL 60:sta. 

Vaihtoehtoja Pythonin merkkijonojen leikkausoperaatioille ovat esimerkiksi `substring`-toiminto kielissä, kuten Java tai C#. 

Pythonissa merkkijonon leikkausoperaatio toimii tehokkaasti, koska se pääsee käsiksi suoraan muistilokatioon, vaikka leikkaus luo uuden merkkijonon eikä muuta alkuperäistä merkkijonoa.

## Katso Myös:

1. Pythonin virallinen dokumentaatio merkkijonoista: https://docs.python.org/3/tutorial/introduction.html#strings
2. Verkkokurssi merkkijonojen alistringeistä: https://www.learnpython.org/en/String_Slicing
3. ALGOL 60:n vaikutus ohjelmointikieliin: https://en.wikipedia.org/wiki/ALGOL_60