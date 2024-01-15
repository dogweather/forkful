---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Fish Shell: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Usein tarvitset tietää merkkijonon pituuden suorittaessasi tiettyjä tehtäviä, kuten tiedostonimen syöttämistä tai vertailua. Fish Shellin avulla tämä tehtävä onnistuu helposti ja nopeasti.

## Miten

Fish Shellin `string length` komento palauttaa annetun merkkijonon pituuden.

```
Fish Shell esimerkki:

string length "Tämä on esimerkkistring"
```
```
Tulostus:

25
```

Komento toimii myös yhdessä muuttujien kanssa:

```
Fish Shell esimerkki:

set merkkijono "Tämä on toinen esimerkkistring"
string length $merkkijono
```

```
Tulostus:

30
```

## Syvemmälle

Fish Shellin `string length` komento käyttää algoritmia, joka laskee merkkien määrän merkkijonossa. Se huomioi myös mahdolliset välilyönnit ja erikoismerkit, mikä tekee siitä luotettavan tavan selvittää merkkijonon pituuden.

## Katso Myös

- [Fish Shellin dokumentaatio](https://fishshell.com/docs/current/index.html)
- [String Length - Linuxize](https://linuxize.com/post/fish-string-length/)