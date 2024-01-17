---
title:                "Alimerkkijonojen erottelu"
html_title:           "Python: Alimerkkijonojen erottelu"
simple_title:         "Alimerkkijonojen erottelu"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Substringien erottaminen tarkoittaa osan erottamista merkkijonosta. Ohjelmoijat tekevät tätä usein silloin, kun he haluavat käsitellä pienempiä osia suuremmista merkkijonoista ja suorittaa tiettyjä toimintoja niille.

## Kuinka tehdä:
```Python 
teksti = "Tervetuloa Pythonin ohjelmointiin!"
print(teksti[10:16])
```
Tuloste: "Python"

```Python 
sana = "moikka"
print(sana[-2:])
```
Tuloste: "ka"

```Python 
nimi = "Matti Meikäläinen"
print(nimi[:5])
```
Tuloste: "Matti"

## Syväsukellus:
Substringeja on ollut mahdollista erottaa ohjelmoijien käytössä olevista kielistä jo pitkään. Pythonissa on käyttäjäystävällisiä keinoja tehdä tämä, kuten näimme yllä. On myös muita tapoja erottaa substringeja, kuten käyttämällä slice-menetelmää tai regular expression -kirjastoa.

## Katso myös:
- [Pythonin dokumentaatio substringeista](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Slice-menetelmä Pythonissa](https://www.w3schools.com/python/python_strings_slicing.asp)
- [Regular Expression -kirjasto Pythonissa](https://docs.python.org/3/library/re.html)