---
title:                "Tarkistetaan, onko hakemisto olemassa"
html_title:           "Elm: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Tarkistetaan Onko Hakemisto Olemassa PHP:ssa?

## Mikä & Miksi?

Hakemiston olemassaolon tarkistaminen on prosessi, jonka avulla näemme, onko tietty hakemisto olemassa tietyssä sijainnissa. Ohjelmoijat tekevät tämän, jotta voidaan välttää virheitä, jotka tapahtuisivat, jos yritämme käyttää hakemistoa, joka ei ole olemassa.

## Miten Näin:

PHP tarjoaa built-in funktion `is_dir()` tarkistamaan, onko hakemisto olemassa. Näin se toimii:

```PHP
<?php

$dir = '/path/to/your/directory';

if (is_dir($dir)) {
    echo "Hakemisto on olemassa";
} else {
    echo "Hakemisto ei ole olemassa";
}

```
Tämä scriptin tulostaa "Hakemisto on olemassa", jos hakemisto on olemassa. Muutoin se tulostaa "Hakemisto ei ole olemassa".

## Deep Dive

Ennen PHP 4:n versiota, `is_dir()` funktio ei ollut saatavilla, joten ohjelmoijat joutivat käyttämään `opendir()` funktiota ja hallitsemaan virheitä tilanteissa, jossa hakemistoa ei ollut.

Vaihtoehtona `is_dir()` funktiolle, voit yrittää avata hakemiston `opendir()` funktion avulla ja tarkistaa, palauttaako se FALSE. Tämä tarkoittaa, ettei hakemisto ole olemassa:

```PHP
<?php

$dir = '/path/to/your/directory';

if (@opendir($dir)) {
    echo "Hakemisto on olemassa";
} else {
    echo "Hakemisto ei ole olemassa";
}
```

Kuitenkin, `is_dir()` on yksinkertaisempi ja suoraviivaisempi tapa saavuttaa sama tulos.

PHP ylläpitää hakemistolistaa hakemistopolkujen ratkaisemiseksi. Kun tarkistat, onko hakemisto olemassa `is_dir()` funktion avulla, PHP tarkistaa ensin tämän sisäisen listan. Jos se ei löydä hakemistoa sieltä, se tarkistaa hakemiston tiedostojärjestelmästä.

## Katso Myös

- PHP Manual: is_dir() [linkki](https://www.php.net/manual/en/function.is-dir.php)
- PHP Manual: opendir() [linkki](https://www.php.net/manual/en/function.opendir.php)
- StackOverflow: How to check if directory exists in PHP [linkki](https://stackoverflow.com/questions/3294918/how-to-check-if-directory-exists-in-php)