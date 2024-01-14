---
title:    "PHP: Komentoriviparametrien lukeminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi lukea komentoriviparametreja?

Komentoriviparametrit ovat erittäin hyödyllisiä työkaluja PHP-ohjelmoijille. Ne antavat mahdollisuuden ohjelmille ottaa vastaan ulkoisia syötteitä ja suorittaa toimintoja niiden perusteella. Tämä voi olla erityisen hyödyllistä, kun ohjelmaan halutaan antaa muuttuvia arvoja, esimerkiksi tietokannan nimi tai käyttäjän syöttämät tiedot. Lue lisää tästä artikkelista, miten voit lukea komentoriviparametreja PHP:ssa.

## Kuinka lukea komentoriviparametreja

Komentoriviparametrien lukeminen PHP:ssa on helppoa. Voit käyttää funktiota `getopt()` ja antaa sille parametrina haluamasi parametrit sekä niiden arvot. Tämän jälkeen voit tarkistaa, onko parametrien arvojen määrittämässä taulukossa haluamasi parametri ja käyttää sitä ohjelman suorittamiseen.

Esimerkiksi, jos haluat ohjelman tulostavan tietokannan nimen, joka on annettu käyttäjän toimesta komentorivillä, voit käyttää seuraavaa koodia:

```PHP
<?php
// Otetaan tietokannan nimi talteen komentoriviltä
$params = getopt("d:");

// Tarkistetaan, löytyykö tietokannan nimi parametreista
if (array_key_exists("d", $params)) {
    $database_name = $params["d"]; // Asetetaan muuttujaan tietokannan nimi
    echo "Tietokannan nimi on " . $database_name; // Tulostetaan nimi
}
```

Tämän esimerkin avulla voit helposti lukea muitakin parametreja ja käyttää niitä oman ohjelmasi toiminnassa.

## Syvempi sukellus

Parametreja lukiessa on hyvä ottaa huomioon muutama seikka. Ensinnäkin, muista tarkistaa, onko parametrien arvot määrittäneen taulukon avain olemassa ennen sen käyttöä. Näin vältät virhetilanteet ja ohjelmasi ei kaadu, jos parametreja ei löydy.

Toiseksi, voit antaa `getopt()`-funktiolle useita parametreja, joista jokaisella voi olla omat arvonsa. Voit myös antaa parametrille useita lyhenteitä, jotka suorittavat saman toiminnon.

Voit lukea lisää `getopt()`-funktion ominaisuuksista PHP:n virallisesta dokumentaatiosta.

## Katso myös

- [PHP:n `getopt()`-funktion virallinen dokumentaatio](https://www.php.net/manual/en/function.getopt.php)
- [Komentoriviparametrit PHP:ssa - TutorialsPoint](https://www.tutorialspoint.com/php/php_command_line.htm)
- [Reading Command Line Arguments with PHP - Code Tutsplus](https://code.tutsplus.com/tutorials/reading-command-line-arguments-with-php--cms-21911)

Kiitos, että luit tämän artikkelin. Toivottavasti se auttoi sinua ymmärtämään komentoriviparametrien lukemista PHP:ssa paremmin. Muista käyttää tätä kätevää työkalua omassa ohjelmoinnissasi!