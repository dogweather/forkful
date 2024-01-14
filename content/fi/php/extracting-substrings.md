---
title:                "PHP: Alimerkkien eristäminen"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Subtringejen erottelu on tärkeä taito jokaiselle PHP-ohjelmoijalle. Se auttaa käsittelemään tekstejä ja muuttamaan niitä tarpeen mukaan. Tämä taito on erityisen hyödyllinen esimerkiksi, kun haluat käsitellä käyttäjien syöttämiä lomakkeiden arvoja tai hakea tietoja tietokannasta.

## Kuinka

PHP tarjoaa muutamia erilaisia toimintoja, joiden avulla voit erottaa substringejä. Yksi yksinkertaisimmista tavoista on käyttää ```substr()```-funktiota, joka ottaa parametreiksi alkuperäisen merkkijonon sekä aloitus- ja lopetuskohdat. Seuraava esimerkki näyttää, kuinka voit käyttää tätä funktiota:

```PHP
$string = "Tämä on esimerkkimerkkijono";
// Eronnetaan ensimmäinen sana
$substring = substr($string, 0, 4);
echo $substring; // Tulostaa "Tämä"
```

Toinen hyödyllinen tapa erottaa substringejä on käyttää ```explode()```-funktiota, joka erottaa merkkijonon halutun merkin tai merkkijonon kohdalta. Tämä on hyödyllinen esimerkiksi, kun haluat erottaa sanat välimerkkien kohdalta. Esimerkiksi:

```PHP
$string = "Tämä, on, erilainen, merkkijono";
$pieces = explode(",", $string);
print_r($pieces); // Tulostaa "Array ( [0] => Tämä [1] => on [2] => erilainen [3] => merkkijono )"
```

## Syvällisempi sukellus

Tämän lisäksi PHP:llä on myös muita toimintoja, kuten ```mb_substr()```, joka toimii samalla tavalla kuin ```substr()```, mutta se ottaa huomioon myös monikieliset merkkijonot. Voit myös käyttää ```preg_match()```-funktiota regexin avulla erottamaan halutut substringit merkkijonosta.

On myös tärkeää huomata, että merkkijonot kerätään PHP:ssä alkiopohjaisiin taulukoihin, mikä voi tehdä niiden käsittelystä helpompaa joissain tilanteissa. Esimerkiksi, jos haluat muuttaa tiettyjä sanoja merkkijonossa, voit ensin erottaa merkkijonon sanoiksi ```explode()```-funktiolla ja sitten tehdä muutoksia taulukon alkioiden avulla.

## Katso myös

- [PHP manual - substr()](https://www.php.net/manual/en/function.substr.php)
- [PHP manual - explode()](https://www.php.net/manual/en/function.explode.php)
- [PHP manual - mb_substr()](https://www.php.net/manual/en/function.mb-substr.php)
- [PHP manual - preg_match()](https://www.php.net/manual/en/function.preg-match.php)