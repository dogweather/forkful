---
title:                "PHP: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia tilanteita, joissa ohjelmoinnissa on tarpeen selvittää merkkijonon pituus. Esimerkiksi tietokannan tiedon tallentamisessa, tekstikenttien validoinnissa tai yksinkertaisesti merkkijonon muotoilun yhteydessä. Siksi on tärkeää ymmärtää miten PHP:ssä löydetään merkkijonon pituus.

## Kuinka
Merkkijonon pituuden löytäminen PHP:ssä on hyvin yksinkertaista, sillä siihen on valmiiksi sisäänrakennettu funktio `strlen()`. Tämä funktio saa parametrinaan halutun merkkijonon ja palauttaa sen pituuden. 

```PHP
$merkkijono = "Tämä on esimerkki";
echo strlen($merkkijono);
```

Tämä koodi tulostaa näytölle luvun 18, sillä merkkijonossa on 18 merkkiä. On kuitenkin tärkeää huomata, että merkkijonon pituus lasketaan myös välilyönnit ja välimerkit mukaan lukien. 

```PHP
$merkkijono = "Tämä on esimerkki!";
echo strlen($merkkijono);
```
Tässä tapauksessa tulostuksena on 19.

## Sukella syvemmälle
Merkkijonon pituuden laskeminen ei aina ole yksiselitteistä, sillä se riippuu käytetystä merkistöstä. Esimerkiksi erikoismerkit, kuten ääkköset, voivat vaikuttaa pituuteen eri tavalla. Siksi on hyvä tarkistaa, että käytetty merkistö sopii tarkoitukseen.

On myös mahdollista muuntaa merkkijono eri muotoon ennen sen pituuden laskemista. Tämä on hyödyllistä esimerkiksi silloin, kun halutaan poistaa välilyönnit merkkijonosta ennen sen pituuden tarkistamista. Tämä onnistuu `trim()`-funktiolla.

```PHP
$merkkijono = " Tämä on esimerkki ";
echo strlen($merkkijono); // Tulostaa 21
echo strlen(trim($merkkijono)); // Tulostaa 18
```

## Katso myös
- PHP:n virallinen dokumentaatio `strlen()`-funktiosta: https://www.php.net/manual/en/function.strlen.php
- Tietoa merkistöistä PHP:ssä: https://www.php.net/manual/en/function.mb-internal-encoding.php
- `trim()`-funktion dokumentaatio: https://www.php.net/manual/en/function.trim.php