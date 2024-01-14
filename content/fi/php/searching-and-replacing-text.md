---
title:                "PHP: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Ehkä olet kokenut sen. Olet kirjoittanut pitkän ja monimutkaisen koodin ja huomaat, että sinun täytyy vaihtaa jokainen esiintyminen tietystä merkkijonosta toiseen. Se voi olla uuvuttavaa, mutta ei hätää - PHP tarjoaa kätevän tavan tehdä tämä prosessi nopeasti ja vaivattomasti.

## Kuinka tehdä

PHP tarjoaa kaksi pääasiallista funktiota tekstin etsimiseen ja korvaamiseen: `str_replace()` ja `preg_replace()`. Molemmat toimivat lähes samalla tavalla, mutta preg_replace() tarjoaa enemmän joustavuutta säännöllisten lausekkeiden kanssa.

Alla on esimerkki koodista, joka korvaa kaikki "hello" esiintymät merkkijonosta "world". Tämän esimerkin oletetaan olevan muuttuja nimeltä $text:

```PHP
str_replace("hello", "world", $text);
```
Seuraava esimerkki käyttää preg_replace() säännöllistä lauseketta etsimään ja korvaamaan "hello" ja sen perään tulevan sanan "there" merkkijonolla "world":

```PHP
preg_replace("/hello\s+\w+/", "world", $text);
```

## Syvempää tietoa

PHP tarjoaa myös muita hyödyllisiä funktioita, kuten `mb_ereg_replace()` ja `strtr()`, joiden avulla voit tehdä monimutkaisempia tekstikorvauksia. On myös tärkeää muistaa, että säännölliset lausekkeet voivat auttaa tekstin etsimisessä ja korvaamisessa, mutta ne voivat myös olla hieman hankalia, joten tutustu niihin huolellisesti ennen niiden käyttöä.

## Katso myös

[Tämä artikkeli](https://www.php.net/manual/en/function.str-replace.php) antaa sinulle lisätietoa PHP:n `str_replace()` -funktiosta ja sen käytöstä.

[Tämä blogikirjoitus](https://www.regular-expressions.info/php.html) tarjoaa kattavan oppaan PHP:n säännöllisiin lausekkeisiin ja niiden käyttöön.

[Tämä Stack Overflow -vastaus](https://stackoverflow.com/questions/5615229/finding-and-replacing-words-in-a-string-using-php) sisältää esimerkkejä monimutkaisemmista tekstikorvauksista PHP:n avulla.