---
title:                "Python: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/python/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit selvittää merkkijonon pituuden? Tämä on tärkeä taito ohjelmoinnissa, sillä se auttaa sinua hallitsemaan ja käsittelemään merkkijonoja tehokkaasti. Merkkijonot ovat tärkeä osa ohjelmointia ja niitä käytetään esimerkiksi tekstin käsittelyssä ja tietokantojen hakemisessa.

## Kuinka

Voit helposti selvittää merkkijonon pituuden Pythonilla. Ensimmäinen askel on luoda muuttuja, joka sisältää haluamasi merkkijonon. Voit tehdä tämän kirjoittamalla seuraavan koodin:

```Python
merkkijono = "Tämä on esimerkki merkkijonosta"
```

Kun olet määrittänyt muuttujan, voit käyttää `len()` funktiota selvittääksesi merkkijonon pituuden. Tämä funktio laskee kaikki merkit merkkijonossa, mukaan lukien välilyönnit ja erikoismerkit. Käytännössä tämä tarkoittaa, että se laskee myös tyhjät välilyönnit, jotka ovat tärkeitä monimutkaisissa tiedonkäsittelytehtävissä.

```Python
pituus = len(merkkijono)
print(pituus)
```

Tämä koodi tulostaa arvon 31, koska merkkijonossa on 31 merkkiä.

## Syvempi sukellus

Merkkijonon pituuden selvittäminen ei rajoitu vain yksittäisiin merkkijonoihin, vaan voit myös laskea pituuden useasta merkkijonosta yhdistämällä ne. Tätä varten voit käyttää yksinkertaista yhteenlaskutoimitusta:

```Python
pituus = len("Tämä on") + len("merkkijonon") + len("yhdistelmä")
print(pituus)
```

Tämä koodi tulostaa edelleen arvon 31, koska yhdessä nämä kolme merkkijonoa muodostavat saman pituisen merkkijonon kuin alkuperäinen esimerkkimme.

[Lisätietoa merkkijonoista Pythonissa](https://www.w3schools.com/python/python_strings.asp)

[Lisätietoa len() funktiosta](https://docs.python.org/3/library/functions.html#len)

## Katso myös

[Tietoa merkkijonojen käsittelystä Pythonissa](https://www.turtlepoint.fi/python-merkkijonot/)

[Pythonin viralliset dokumentaatiot](https://docs.python.org/3/)