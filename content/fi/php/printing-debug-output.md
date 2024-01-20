---
title:                "Debug-tulosteen tulostaminen"
html_title:           "Bash: Debug-tulosteen tulostaminen"
simple_title:         "Debug-tulosteen tulostaminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/printing-debug-output.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

Tulostusvälineet tekevät ohjelmistoon syötteitä ja tulosteita näkyviksi, mitä kutsutaan debug-tulostukseksi. Ne ovat kriittisen tärkeitä ohjelmointi-virheiden löytämisessä ja korjaamisessa.

---

## Näin se tehdään:

PHP:n käyttöön on saatavilla useita debug-tulostuksen työkaluja. Yksinkertaisin niistä on `echo` ja `print` funktiot.

```PHP
<?php
$muuttuja = "Moi Suomi!";
echo $muuttuja; // Tulostaa: Moi Suomi!
?>
```

Voimme myös käyttää `var_dump` funktiota tarkastellaksemme muuttujan tietoja.

```PHP
<?php
$muuttuja = array("Helsinki", "Espoo", "Vantaa");
var_dump($muuttuja);
?>
// Tulostaa: array(3) { [0]=> string(8) "Helsinki" [1]=> string(5) "Espoo" [2]=> string(6) "Vantaa" }
```

Konsoliin voi tulostaa myös `print_r` funktiolla:

```PHP
<?php
$muuttuja = array("Jyväskylä", "Turku", "Oulu");
print_r($muuttuja);
?>
// Tulostaa: Array ( [0] => Jyväskylä [1] => Turku [2] => Oulu )
```

---

## Syvempi sukellus

PHP:n debug-tulostuksen työkalut ovat tulleet pitkän matkan. Ensimmäisissä PHP-versioissa `echo` ja `print` olivat yleisimmin käytössä. Saatavilla on monia muita työkaluja, kuten Xdebug ja PHP_Debug, jotka tarjoavat monipuolisia toimintoja ja ovat tehokkaita isojen projektien debuggauksessa.

On olemassa myös muita PHP debug -teknologioita kuin printtaus. Esimerkiksi Interactive PHP Debugging (`php -a`) antaa sinun suorittaa koodia komentoriviltä, hyödyllinen esimerkiksi silloin kun haluat testata pienen koodinpätkän toimivuutta nopeasti. 

---

## Katso myös

[Xdebug](https://xdebug.org/): Tehokas ja monipuolinen PHP debuggaustyökalu.

[PHP_Debug](https://pear.php.net/package/PHP_Debug): PEAR paketin PHP debuggaustyökalu.

[PHP virallinen dokumentaatio](https://www.php.net/manual/en/debugger.php): Lue lisää PHP:n sisäänrakennetuista debug-työkaluista.

[PHPUnit](https://phpunit.de/): Yksikkötestaus on usein paras tapa debugata. PHPUnit on standardi Työkalu PHP:ssä.