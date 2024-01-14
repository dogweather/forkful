---
title:    "PHP: Tekstin etsiminen ja korvaaminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi etsiminen ja korvaaminen on tärkeää PHP-ohjelmoinnissa?

Tekstin etsiminen ja korvaaminen on tärkeä osa PHP-ohjelmointia, koska se säästää aikaa ja vaivaa käsiteltäessä suuria määriä tekstiä. Tämä toiminto auttaa myös varmistamaan, että tiettyä tekstiä esiintyy halutuissa kohdissa koodissa.

## Kuinka etsiä ja korvata tekstiä PHP:ssa?

PHP:ssa on useita toimintoja, jotka mahdollistavat tekstin etsimisen ja korvaamisen. Yksi niistä on `str_replace()` -funktio, joka etsii ja korvaa tietyn merkkijonon toisella merkkijonolla. Esimerkiksi:

```php
$text = "Tervetuloa maailmaan, PHP!";
$new_text = str_replace("maailmaan", "koodaukseen", $text);
echo $new_text;
```

Tämä tulostaa "Tervetuloa koodaukseen, PHP!". Jos haluat korvata vain ensimmäisen esiintymän, voit käyttää `str_replace_first()` -funktiota. Voit myös käyttää `str_ireplace()` -funktiota, joka etsii ja korvaa tekstiä välittämättä kirjainkoosta.

## Syvemmälle tekstin etsimiseen ja korvaamiseen PHP:ssa

PHP:ssa on myös muita toimintoja, jotka mahdollistavat tekstin etsimisen ja korvaamisen, kuten `preg_replace()` -funktio, joka käyttää säännöllisiä lausekkeita löytääkseen ja korvatakseen tekstin. Tämä toiminto on erittäin hyödyllinen monimutkaisempien tekstipalasten etsimiseen ja korvaamiseen.

Voit myös käyttää `str_replace()` -funktiota yhdessä `foreach` -silmukan kanssa käsitelläksesi useita tekstin paloja kerralla. Tämä on hyödyllistä esimerkiksi käyttäjän syötteiden validoinnissa tai tietokannasta haetun datan käsittelyssä.

## Katso myös

- [PHP:n virallinen manuaali tekstien etsimisestä ja korvaamisesta](https://www.php.net/manual/en/function.str-replace.php)
- [Aloittelijaystävällinen opas PHP:seen](https://dev.to/koderockinen/the-beginners-guide-to-php-3eoi)
- [Yleisimmät PHP:n säännölliset lausekkeet](https://www.php.net/manual/en/function.preg-replace.php) (englanniksi)