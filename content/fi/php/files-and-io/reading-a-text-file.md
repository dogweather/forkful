---
date: 2024-01-20 17:55:07.792215-07:00
description: "How to: - Kuinka: PHP tarjoaa useita tapoja lukea tekstitiedostoja.\
  \ Alla on suoraviivainen esimerkki `file_get_contents` toiminnosta ja iteroinnista\u2026"
lastmod: '2024-04-05T21:53:58.245157-06:00'
model: gpt-4-1106-preview
summary: ''
title: Tekstitiedoston lukeminen
weight: 22
---

## How to: - Kuinka:
PHP tarjoaa useita tapoja lukea tekstitiedostoja. Alla on suoraviivainen esimerkki `file_get_contents` toiminnosta ja iteroinnista tiedoston riveille `file` toiminnolla.

```PHP
<?php
// Luetaan koko tiedosto yhtenä merkkijonona
$tiedostonSisalto = file_get_contents('esimerkki.txt');
echo $tiedostonSisalto;

// Luetaan tiedosto rivi riviltä taulukkoon
$rivit = file('esimerkki.txt');
foreach ($rivit as $rivi) {
    echo $rivi;
}
?>
```

Tuloste riippuu `esimerkki.txt` tiedoston sisällöstä. Jos tiedostossa on teksti "Hei maailma!", ensimmäinen koodi tulostaa koko lauseen ja toinen koodi tulostaa sen rivi kerrallaan.

## Deep Dive - Syväsukellus:
Tiedoston lukeminen PHP:ssä ei ole uusi konsepti; se on ollut osa kieltä alusta alkaen. Alkuaikoina käytössä oli perusfunktioita kuten `fopen` ja `fgets`, joilla tiedosto avataan ja luetaan rivi kerrallaan. 

Nykyään on useampia käteviä vaihtoehtoja:
- `readfile()` - Echoes tiedoston sisällön suoraan
- `file()` - Palauttaa tiedoston rivit taulukkona
- `fopen()` ja `fread()` - Monipuolisin tapa, joka mahdollistaa hienojakoisen kontrollin lukuoperaatioihin

Tiedoston lukuun vaikuttavat myös palvelimen oikeudet, tiedoston koko ja muoto. Suurten tiedostojen kanssa kannattaa käyttää 'fopen' ja looppia, joka lukee tiedoston osissa; tämä säästää muistia.

## See Also - Katso Myös:
- PHP:n virallinen dokumentaatio `file_get_contents`: https://www.php.net/manual/en/function.file-get-contents.php
- PHP:n virallinen dokumentaatio `file`: https://www.php.net/manual/en/function.file.php
- Stack Overflow -keskustelut tiedostojen käsittelystä: https://stackoverflow.com/questions/tagged/php+file-get-contents
- Tutorial tutustumiseen tiedostojen käsittelyyn PHP:ssä: https://www.tutorialspoint.com/php/php_file_handling.htm
- PHP.net-käsikirja `fopen` ja lukutoimintoihin liittyen: https://www.php.net/manual/en/function.fopen.php
