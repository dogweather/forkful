---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "PHP: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi löytää merkkijonon pituuden? Yksinkertaisesti siksi, että verkkokehityksessä ja ohjelmoinnissa on usein tarpeen työskennellä tekstipohjaisten tietojen kanssa, ja niiden pituuden tunteminen on olennainen osa tätä prosessia.

## Kuinka

Käyttämällä PHP:n `strlen()` -funktiota voit helposti löytää merkkijonon pituuden. Katso seuraava esimerkki:

```PHP
$merkkijono = "Tämä on testi";
echo strlen($merkkijono);
```

Tulostus: `13`

`strlen()`-funktio ottaa argumenttina merkkijonon ja palauttaa sen pituuden kokonaislukuna. Tämä on yksinkertainen, mutta hyödyllinen tapa löytää merkkijonon pituus.

## Syväyhteensä

On tärkeää huomata, että `strlen()` laskee myös tyhjät välilyönnit merkkijonon pituuteen. Esimerkiksi seuraavassa tapauksessa:

```PHP
$merkkijono = "Tämä on testi";
echo strlen($merkkijono);
```

Tulostus: `14`

Tämä johtuu siitä, että välilyönti on myös merkki ja siten se lasketaan mukaan pituuteen. Tämä saattaa aiheuttaa sekaannusta, mutta on tärkeää muistaa tämä ominaisuus merkkijonon pituutta laskettaessa.

Toinen hyödyllinen funktio merkkijonon pituuteen liittyen on `mb_strlen()`, joka ottaa huomioon myös monikieliset merkit ja palauttaa oikean pituuden myös näissä tapauksissa.

## Katso myös

- [PHP Manual – strlen()](https://www.php.net/manual/en/function.strlen.php)
- [PHP Manual – mb_strlen()](https://www.php.net/manual/en/function.mb-strlen.php)