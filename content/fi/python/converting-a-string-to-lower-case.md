---
title:                "Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "Python: Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Muuttujien merkkijonotyyppien muuttaminen pieniksi kirjaimiksi tarkoittaa, että kaikki merkkijonossa olevat isot kirjaimet muutetaan pieniksi kirjaimiksi. Ohjelmoijat tekevät tämän joukon vertailutehtävissä, jotta voidaan välttää isot ja pienet kirjaimet aiheuttama herkkyys.

## Kuinka:
Voit helposti muuttaa Python merkkijonon pieniksi kirjaimiksi `lower()` funktion avulla. Katso alla oleva esimerkki:
```Python
merkkijono = "Hello, World!"
pienet_kirjaimet = merkkijono.lower()
print(pienet_kirjaimet)
```
Tulostus:
```Python
'hello, world!'
```

## Syvä sukellus:
Merkkijonon muuttaminen pieniksi kirjaimiksi on pitkän aikavälin ohjelmointitekniikka, joka palvelee monia kielestä riippumattomia käyttötarkoituksia, kuten datan normalisointi, jos haluat automatisoida vertailuja, etsintöjä tai lajitteluja.

Vaihtoehtoisesti voit käyttää `casefold()` -funktiota, joka on yleensä aggressiivisempi. Se muuttaa merkkijonot sopimaan paremmin vertailuihin, jotka eivät halua ennakoida isoja ja pieniä kirjaimia.

```Python
merkkijono = "Äiti Ja Isä"
pienet_kirjaimet = merkkijono.casefold()
print(pienet_kirjaimet)
```
Tulostus:
```Python
'äiti ja isä'
```
Jotkut kielet, kuten saksa, käyttävät "skarped s": tää (ß), joka muuttuu "ss": ään `lower()` -funktion kanssa, mutta pysyy samana `casefold()` -funktion kanssa.

Python toteuttaa nämä toiminnot sisäisesti C-kielen standardikirjaston avulla, joka on määritelty eri tavalla eri alustoilla ja voi tuottaa erilaisia tuloksia.

## Katso myös:
- Pythonin virallinen dokumentaatio: [String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Vertailu eri string-funktioiden välillä: [casefold vs lower](https://www.geeksforgeeks.org/python-casefold-vs-lower/)