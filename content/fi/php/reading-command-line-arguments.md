---
title:                "PHP: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Miksi Käytämme PHP-ohjelmointia komentoriviparametrien lukemiseen?

Komentoriviparametrien lukeminen on tärkeä osa PHP-ohjelmointia. Se antaa meille mahdollisuuden suorittaa erilaisia toimintoja ohjelman suorituksen aikana, kuten määrittää haluttuja arvoja tai suorittaa erilaisia toimintoja riippuen siitä, mitä parametreja käyttäjä antaa ohjelmalle. Tämä tekee ohjelmistostamme interaktiivisemman ja monipuolisemman.

## Kuinka käyttää PHP:ta komentoriviparametrien lukemiseen?

PHP-ohjelmointikieli tarjoaa meille kolme tapaa lukea komentoriviparametreja: $_SERVER-muuttujan kautta, getopt-funktion ja getopt_long-funktion avulla. Käymme läpi jokaisen alla olevilla esimerkeillä ja näytämme, miten ne toimivat.

### $_SERVER-muuttuja

Muuttuja $_SERVER sisältää tietoja käyttäjän lähettämistä HTTP-pyyntöistä. Käytämme tätä muuttujaa lukemaan komentoriviparametreja seuraavasti:

```PHP
<?php
// Esimerkki $_SERVER-muuttujan käytöstä
foreach($_SERVER['argv'] as $param) {
    echo $param . "\n";
}
?>
```

Tämä koodi tulostaa kaikki käyttäjän antamat parametrit riveittäin.

### getopt-funktio

getopt-funktio on hyödyllinen, kun haluamme lukea vain tiettyjä parametreja ja suorittaa niihin liittyviä toimintoja. Se toimii siten, että ensimmäisenä argumenttina annetaan merkkijono, joka kertoo, mitkä parametrit haluamme lukea. Esimerkiksi merkkijono "hf:" tarkoittaa, että haluamme lukea parametrit -h ja -f, ja että -f-parametrille on määritelty arvo.

```PHP
<?php
// Esimerkki getopt-funktion käytöstä
$parameters = getopt("hf:");

if (array_key_exists("h", $parameters)) {
    // Suoritetaan toiminto, jos käyttäjä on antanut -h parametrin
}

if (array_key_exists("f", $parameters)) {
    // Suoritetaan toiminto, jos käyttäjä on antanut -f parametrin
    $file = $parameters["f"];
}
?>
```

### getopt_long-funktio

getopt_long-funktio toimii samalla tavalla kuin getopt-funktio, mutta se tukee pidempiä parametreja. Se ottaa vastaan samat argumentit kuin getopt-funktio ja palauttaa samalla tavalla muotoillun taulukon.

## Syvällisempi katsaus komentoriviparametrien lukemiseen

Komentoriviparametrien lukeminen on hyödyllinen toiminto kaikentyyppisissä PHP-ohjelmoinnissa. Se antaa meille mahdollisuuden käyttää interaktiivisia työkaluja ja vaihtaa asetuksia ohjelmiston suorituksen aikana. Lisäksi parametrien lukeminen voi auttaa meitä tunnistamaan ja käsittelemään virheitä ohjelman suorituksen aikana.

## Katso myös

- [PHP:n virallinen dokumentaatio komentoriviparametrien lukemisen](https://www.php.net/manual/en/reserved.variables.server.php) osalta
- [PHP:n getopt-funktion dokumentaatio](https://www.php.net/manual/en/function.getopt.php)
- [PHP:n getopt_long-funktion dokumentaatio](https://www.php.net/manual/en/function.getopt-long.php)