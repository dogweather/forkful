---
title:                "Merkkijonon interpolointi"
date:                  2024-01-20T17:51:41.128538-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon interpolointi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Stringin interpolointi tarkoittaa muuttujien ja lausekkeiden upottamista merkkijonoihin. Se tekee koodista luettavampaa ja dynaamisempaa, helpottaen muuttuvan datan esittämistä.

## Miten:
```Python
# Python 3.6+
nimi = "Pekka"
ikä = 42
print(f"Hei, nimeni on {nimi} ja olen {ikä} vuotta vanha.")

# Tuloste:
# Hei, nimeni on Pekka ja olen 42 vuotta vanha.

# Vanhemmissa versioissa:
print("Hei, nimeni on {} ja olen {} vuotta vanha.".format(nimi, ikä))
```

## Syväsukellus:
Interpolointi on ollut käytössä vanhemmissa ohjelmointikielissä, esim. Perl ja PHP. Pythonissa `.format()` metodi tuli käyttöön ensin, f-merkkijonot (formatted string literals) tulivat Python 3.6:ssa. Muita tapoja ovat %-formatointi ja template strings, mutta f-merkkijonot ovat suositumpia selkeyden ja lyhyyden vuoksi. Teknisenä yksityiskohtana, f-merkkijonot käsitellään suoritusajassa, mahdollistaen lausekkeiden dynaamisen arvioinnin merkkijonon sisällä.

## Katso Myös:
- Pythonin virallinen dokumentaatio f-merkkijonoista: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- PEP 498, joka esittelee f-merkkijonot: https://www.python.org/dev/peps/pep-0498/
- Pythonin `.format()` metodi: https://docs.python.org/3/library/stdtypes.html#str.format
- String Template Class: https://docs.python.org/3/library/string.html#template-strings