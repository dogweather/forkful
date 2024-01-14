---
title:                "PHP: Alirivien poimiminen"
simple_title:         "Alirivien poimiminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Substringien louhinta on erittäin hyödyllistä, kun haluat käsitellä ja manipuloida merkkijonoja PHP:ssä. Se voi auttaa sinua saamaan tarkemman otannan tiedoista tai suodattamaan haluamasi tiedot tietokannassa.

## Miten tehdä

PHP:ssä substringien louhinta voidaan tehdä useilla eri tavoilla, mutta yksi yleisimmistä on käyttää `substr()` -funktiota. Voit käyttää sitä seuraavalla tavalla:

```PHP
$string = "Tämä on esimerkkilause.";
// Louhitaan ensimmäinen 8 merkkiä
$louhittu = substr($string, 0, 8);
// Tulostetaan tulos
echo $louhittu; //Tämä on
```

`substr()` -funktio ottaa kolme parametria: alkuperäinen merkkijono, aloitusindeksi ja haluttu pituus. Tämä tarkoittaa, että voit louhia merkkijonosta haluamasi määrän merkkejä haluamastasi kohdasta.

Voit myös käyttää `mb_substr()` -funktiota, jota suositellaan käytettäväksi UTF-8 -merkkikoodauksen kanssa, jotta vältetään mahdolliset ongelmat monibyte -merkkien kanssa. Se toimii samalla tavalla kuin `substr()`, mutta se huomioi merkistön monimutkaisuuden.

## Syvempi sukellus

PHP tarjoaa myös muita tapoja louhia substringeja, kuten `str_replace()` ja `preg_replace()` -funktiot. Näiden avulla voit louhia ja korvata tiettyjä merkkejä tai osia merkkijonosta.

On tärkeää huomata, että substringien louhinta voi olla tehokas, mutta se voi myös hidastaa suorituskykyä ja aiheuttaa turhia kustannuksia. Varmista siis aina, että käytät sitä oikeissa tilanteissa ja optimoit koodisi asianmukaisesti.

## Katso myös

- PHP:n `substr()` -dokumentaatio: https://www.php.net/manual/en/function.substr.php
- `mb_substr()` vs `substr()`: https://stackoverflow.com/questions/6684776/use-of-mb-substr-in-php
- `str_replace()` dokumentaatio: https://www.php.net/manual/en/function.str-replace.php
- `preg_replace()` dokumentaatio: https://www.php.net/manual/en/function.preg-replace.php