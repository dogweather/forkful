---
title:                "PHP: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Stringien yhdistäminen on tärkeä osa PHP-ohjelmointia, ja se auttaa luomaan dynaamisia ja joustavia sovelluksia. Stringien yhdistäminen antaa mahdollisuuden yhdistää erilaisia tietoja ja muuttujia yhteen ja luoda näin monipuolisia sisältöjä.

## Miten tehdä

Stringien yhdistäminen tapahtuu PHP:ssa käyttäen pistettä ".". Seuraavassa esimerkissä meillä on muuttuja "etunimi" ja "sukunimi", ja haluamme yhdistää ne yhdeksi stringiksi:

```PHP
$etunimi = "Matti";
$sukunimi = "Meikäläinen";
echo $etunimi . " " . $sukunimi;
```

Tulostus:

```
Matti Meikäläinen
```

Voimme myös yhdistää useita muuttujia yhteen ja sisällyttää välissä haluamamme teksti, kuten seuraavassa esimerkissä:

```PHP
$ika = 42;
echo "Olen " . $ika . " vuotta vanha.";
```

Tulostus:

```
Olen 42 vuotta vanha.
```

## Syvemmälle

Stringien yhdistämisellä on monia käyttötarkoituksia. Sitä voi käyttää esimerkiksi dynaamisten sivujen luomiseen, joissa haluamme yhdistää tietokannasta haetut tiedot HTML:ään. Stringien yhdistäminen on myös välttämätöntä, kun haluamme luoda linkkejä tai polkuja tiedostojen välillä.

On hyvä muistaa, että stringien yhdistäminen voi olla tehoton tapa luoda isoja ja monimutkaisia stringeja. Tällöin kannattaa harkita käytettävän PHP:n sulkemista, joka on nopeampi ja tehokkaampi tapa yhdistää merkkijonoja.

## Katso myös

- [PHP.net: Tietojen yhdistäminen (String Concatenation)](https://www.php.net/manual/en/language.operators.string.php)
- [W3Schools: PHP stringien yhdistäminen](https://www.w3schools.com/php/php_operators.asp)
- [Codeacademy: Stringien yhdistäminen](https://www.codecademy.com/learn/learn-php/modules/learn-php-strings/cheatsheet)