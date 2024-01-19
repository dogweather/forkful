---
title:                "Merkkijonon pääkirjaintaminen"
html_title:           "Python: Merkkijonon pääkirjaintaminen"
simple_title:         "Merkkijonon pääkirjaintaminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Tekstijonon "capitalisoiminen" tarkoittaa sen muuttamista niin, että jokainen sanan ensimmäinen kirjain on suuri (esimerkiksi 'hello world' muuttuu 'Hello World'). Tätä tarvitaan tyypillisesti, kun halutaan esittää tekstiä ihmisille lukemisen helpottamiseksi tai tietyt formaatit vaativat sitä.

## Miten teen sen:

Pythonissa stringin capitalisointi on erittäin helppoa. Tässä on esimerkki miten se tapahtuu:

```Python
s = 'hello world'
capitalized_s = s.title()
print(capitalized_s)
```

Suorittamalla tämän koodin saatte tulostuksen "Hello World".

## Syvempi sukellus:

Historiallinen yhteys - Pythonin 'title()' -metodi, jota käytetään merkkijonon capitalisoimiseen, lisättiin Python 2.6 -versiossa sujuvoittamaan tekstinkäsittelyä.

Vaihtoehdot - Vaikka 'title()' on suosittu tapa tehdä tämä, on myös muita vaihtoehtoja. Yksi yleisimmin käytetty on 'capitalize()', mutta se tekee suureksi vain merkkijonon ensimmäisen kirjaimen.

```Python
s = 'hello world'
capitalized_s = s.capitalize()
print(capitalized_s)
```

Tämän koodin tuotoksen on "Hello world".

Implementaation yksityiskohdat - Pythonin 'title()' -metodin toiminnan ymmärtämiseen tulee tietää, että se käy merkkijonon läpi ja muuttaa jokaisen sanan ensimmäisen kirjaimen suureksi, samalla muuttaen loput kirjaimet pieniksi.

## Katso myös:

Pythonin virallinen dokumentaatio tarjoaa paljon tietoa tekstinkäsittelyfunktioista:

- 'title()' -metodin dokumentaatio: https://docs.python.org/3/library/stdtypes.html#str.title
- 'capitalize()' -metodin dokumentaio: https://docs.python.org/3/library/stdtypes.html#str.capitalize
- Lisää tekstinkäsittelytoimintoja: https://docs.python.org/3/library/stdtypes.html#string-methods