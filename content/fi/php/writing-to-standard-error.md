---
title:                "PHP: Kirjoittaminen standardivirheelle"
programming_language: "PHP"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Miksi

PHP-ohjelmointikieli tarjoaa useita erilaisia tapoja tulostaa tietoa. Eräs näistä on standard error, joka on kätevä tapa näyttää virheilmoituksia ja muuta tärkeää tietoa koodin suorittamisen aikana. Tässä artikkelissa opit, miten kirjoitat standard error -tulostuksia PHP:n avulla.

## Miten

Usein standard error -tulostuksia käytetään virheilmoitusten näyttämiseen. Tämä tapahtuu yleensä silloin, kun jotain on mennyt pieleen koodissa ja haluat ilmoittaa siitä käyttäjälle. Voit kirjoittaa tulostuksen käyttämällä `fwrite()` -funktiota ja `STDERR` -muuttujaa.

```
PHP
<?php
// Luo viesti
$error_message = "Jotain meni pieleen!";

// Kirjoita viesti standard error -tulostukseen
fwrite(STDERR, $error_message);
?>
```
Koodi tulostaa `Jotain meni pieleen!` standard error -tulostuksena.

Voit myös käyttää `print_r()` -funktiota tulostamaan monimutkaisempia tietoja standard error -tulostuksena. Alla olevassa esimerkissä tulostetaan assosiatiivinen taulukko.

```
PHP
<?php
// Luo assosiatiivinen taulukko
$player = array("nimi" => "Mikko", "ikä" => 25, "joukkue" => "HIFK");

// Tulosta taulukko standard error -tulostuksena
print_r($player, true);
?>
```
Tämä tulostaa seuraavan viestin standard error -tulostuksena:

```
Array
(
    [name] => Mikko
    [age] => 25
    [team] => HIFK
)
```

## Syvemmälle

Standard error -tulostuksia käytetään usein yhdessä `try`- ja `catch` -lohkojen kanssa koodin virheiden käsittelyssä. Voit esimerkiksi laittaa `try`-lohkon sisään koodin, joka voi aiheuttaa virheitä, ja sen sisällä olevan `catch`-lohkon avulla voit kirjoittaa virheilmoituksia standard error -tulostukseen.

```
PHP
<?php
try {
    // Koodia, joka voi aiheuttaa virheitä
} catch(Exception $e) {
    // Kirjoita virheilmoitus standard error -tulostukseen
    fwrite(STDERR, $e->getMessage());
}
?>
```

Muista myös, että standard error -tulostusta voi käyttää myös muissa tilanteissa, joissa haluat näyttää tärkeitä tietoja käyttäjälle. Esimerkiksi silloin, kun haluat testata ja debugata koodia, on hyödyllistä kirjoittaa tietoja standard error -tulostukseen, jolloin ne näkyvät selkeästi koodia suorittaessa.

## Katso myös

- [PHP fwrite() -funktio](https://www.php.net/manual/en/function.fwrite.php)
- [PHP print_r() -funktio](https://www.php.net/manual/en/function.print-r.php)
- [PHP try-catch -lohkot](https://www.php.net/manual/en/language.exceptions.php)