---
title:                "Merkkijonon pituuden löytäminen"
html_title:           "Elm: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa tarvitsemme tietoa merkkijonon pituudesta, kuten käyttäjän syötteen tarkistamisessa tai tekstin muotoilussa. Elm-ohjelmointikielellä tämän toiminnon toteuttaminen on helppoa ja tehokasta.

## Kuinka

```Elm
length : String -> Int
```

Ensimmäinen askel merkkijonon pituuden löytämisessä on käyttää sisäänrakennettua `length` -funktiota, joka ottaa merkkijonon parametrina ja palauttaa sen pituuden Int-tyypissä. Esimerkiksi:

```Elm
length "Tämä on esimerkkimerkkijono" --> 26
```

## Syvempi sukellus

Vaikka Elm tarjoaa kätevän sisäänrakennetun `length` -funktion, on hyödyllistä ymmärtää, miten tämä toiminto toimii taustalla. Merkkijono koostuu merkeistä, ja `length` -funktio laskee näiden merkkien määrän ja palauttaa sen pituuden. Voit myös käyttää `length` -funktion sijasta `String.toList` -funktiota ja lukea palautetun listan pituuden.

```Elm
String.toList "Hei" --> [ 'H', 'e', 'i' ]
List.length [ 'H', 'e', 'i' ] --> 3
```

## Katso myös

- [Elm-kielen virallinen dokumentaatio](https://guide.elm-lang.org/)
- [Merkkijonon käsittely Elmissä](https://www.oliverandcode.com/blog/elm/working-with-strings-in-elm/) tutorial