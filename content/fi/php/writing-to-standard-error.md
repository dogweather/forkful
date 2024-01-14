---
title:    "PHP: Kirjoittaminen standardivirheelle"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

Monet PHP-ohjelmoijat saattavat ihmetellä, miksi heidän pitäisi kirjoittaa standardeihin virheisiin. Tämä ominaisuus on kuitenkin erittäin hyödyllinen, kun halutaan kirjoittaa virheitä tai varoituksia ohjelman suorituksen aikana.

## Miten

Kun käytät PHP:ta, voit kirjoittaa virheilmoituksia standardiin virhevirtaan käyttämällä `error_log()`-funktiota. Tämä ei kuitenkaan ole aina tarpeellista, varsinkin jos sinulla on erilaiset loggausmenetelmät käytössä. Siksi voit ohjata virheilmoitukset standardiin virhevirtaan myös muuttamalla php.ini-tiedostoa. Tämä voidaan tehdä lisäämällä vaihtoehtoinen tie `log_errors`-asetukseen.

```PHP
<?php
// Ohjaa virheilmoitukset standardiin virhevirtaan
ini_set('log_errors', 1);
ini_set('error_log', '/var/log/php-error.log');
```

Jos haluat mukauttaa virheilmoituksia enemmän, voit käyttää `error_reporting()`-funktiota määrittämään, mitkä virheet ja varoitukset haluat tallentaa.

```PHP
<?php
// Tallenna vain tietyt virheet
error_reporting(E_ERROR | E_WARNING);
```

## Syvempi sukellus

Kirjoittaminen standardiin virhevirtaan voi auttaa sinua havaitsemaan ja korjaamaan ongelmia ohjelmasi suorituksen aikana. Voit myös käyttää `error_reporting()`-funktiota luomaan omia virhe- ja varoitusviestejä, jotta voit helpommin selvittää, missä osassa koodia virhe tapahtui.

PHP:lla on myös useita tapoja tallentaa ja lukea virheilmoituksia, kuten käyttämällä `try`/`catch`-lohkoja tai `set_exception_handler()`-funktiota.

## Katso myös

- [PHP:n virheiden hallinta](https://www.php.net/manual/en/function.error-log.php)
- [PHP-tietueiden loggaus](https://www.php.net/manual/en/book.syslog.php)
- [Virhe- ja varoitusluokat PHPssa](https://www.php.net/manual/en/errorfunc.constants.php)