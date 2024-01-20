---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Stringin pääkirjaimet tarkoittaa tekstijonon muuttamista niin, että kirjaimet ovat isoina - kuten otsikoissa tai nimissä. Koodarit käyttävät sitä parantaakseen luettavuutta ja standardoimaan datan ulkoasua.

## How to - Näin teet:
Pythonissa stringin saa isolla käyttäen `upper()`-metodia. Pienillä kirjaimilla `lower()` ja alkukirjain isoksi `capitalize()`. Katsotaanpa esimerkkejä.

```Python
# Tekstin muuttaminen kokonaan suuraakkosiksi
text = "tervetuloa ohjelmointimaailmaan"
print(text.upper())
# Output: TERVETULOA OHJELMOINTIMAAILMAAN

# Jokaisen sanan alkukirjaimen muuttaminen isoksi
title = "python ohjelmoinnin perusteet"
print(title.title())
# Output: Python Ohjelmoinnin Perusteet

# Vain ensimmäisen kirjaimen muuttaminen isoksi
greeting = "moi kaikille"
print(greeting.capitalize())
# Output: Moi kaikille
```

## Deep Dive - Syväsukellus
Stringien pääkirjainten historia juontaa juurensa kirjoituskoneista ja varhaisesta tietojenkäsittelystä, jossa erottelu isoilla kirjaimilla oli tarpeen. Myös nykypäivänä esimerkiksi osoitteet ja henkilönimet standardoidaan usein pääkirjaimin.

Kielitieteilijät puhuvat "title casesta", joka on iso alkukirjain jokaisessa merkittävässä sanassa. Pythonissa tämä toimii `title()`-metodilla. Jotkut käyttävät myös `str.capitalize()`-metodia, joka kuitenkin suurentaa vain ensimmäisen kirjaimen.

Alkuperäiset funktiot `upper()`, `lower()` ja `capitalize()` ovat osa Pythonin standardikirjastoa, ja niiden tehtävä on tekstikäsittely. Ne toimivat Unicode-merkkijonoihin, jossa kirjaimet voivat olla monimutkaisempia ja isot/pienet kirjaimet eivät aina määräydy yksiselitteisesti.

Python-koodareille on tarjolla myös kolmansien osapuolten kirjastoja, kuten `stringcase`, joka sisältää lisää tapoja muokata merkkijonoja eri tapauksiin.

## See Also - Katso Myös
- Pythonin virallinen dokumentaatio string-metodeista: [Python 3 string methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- Unicode ja python: [Unicode HOWTO](https://docs.python.org/3/howto/unicode.html)
- String formatting in Python: [PyFormat](https://pyformat.info/)