---
title:    "PHP: Tekstijonojen yhdistäminen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi

Monissa PHP-ohjelmoinnissa on tarve yhdistää tai liittää erilaisia merkkijonoja. Tämä voi esimerkiksi olla tarpeellista, kun halutaan luoda dynaaminen viesti tai muokata olemassa olevaa tekstiä. Tätä kutsutaan merkkijonojen konkatenaatioksi ja se on hyödyllinen taito, joka kannattaa oppia.

## Miten

PHP tarjoaa useita eri tapoja konkatenaation tekemiseen. Yksi tapa on käyttää piste-operaattoria (.), joka yhdistää kaksi merkkijonoa yhdeksi.

```
PHP
$merkkijono1 = "Tämä on";
$merkkijono2 = "esimerkki";
echo $merkkijono1 . $merkkijono2;
// Tulostaa: Tämä on esimerkki
```

Toinen tapa on käyttää konkatenaatiomerkkiä (.=), joka yhdistää uuden merkkijonon jo olemassa olevaan muuttujaan.

```
PHP
$merkkijono = "Tämä on";
$merkkijono .= " esimerkki";
echo $merkkijono;
// Tulostaa: Tämä on esimerkki
```

Merkkijonojen yhdistäminen on myös mahdollista käyttäen sprintf-funktiota, joka antaa enemmän joustavuutta muotoilussa.

```
PHP
$nimi = "Matti";
$ika = 30;
$viesti = sprintf("Hei, olen %s ja olen %d vuotta vanha.", $nimi, $ika);
echo $viesti;
// Tulostaa: Hei, olen Matti ja olen 30 vuotta vanha.
```

## Syvällinen sukellus

Merkkijonojen konkatenaatio ei rajoitu pelkästään perusmerkkijonoihin, vaan sitä voidaan käyttää myös esimerkiksi muuttujien ja muiden PHP-ominaisuuksien yhdistämiseen.

```
PHP
$luku1 = 10;
$luku2 = 5;
$yhteensa = "Yhteenlaskun tulos on: " . ($luku1 + $luku2);
echo $yhteensa;
// Tulostaa: Yhteenlaskun tulos on: 15
```

## Katso myös

- [PHP:n merkkijonofunktiot](https://www.php.net/manual/en/ref.strings.php)
- [Perusteet PHP:n merkkijonoilla työskentelystä](https://www.w3schools.com/php/php_strings.asp)
- [PHP:n merkkijonojen formatointi](https://www.php.net/manual/en/function.sprintf.php)