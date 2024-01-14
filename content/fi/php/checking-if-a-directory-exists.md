---
title:                "PHP: Tarkista onko hakemistoa olemassa"
simple_title:         "Tarkista onko hakemistoa olemassa"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi tarkistaa, onko hakemistoa olemassa?

Hakemistojen olemassaolon tarkistaminen on tärkeä osa PHP-ohjelmointia. Tämä toiminto mahdollistaa ohjelmoijille tarkistaa ja hallita hakemistoja, jotka sisältävät tärkeitä tiedostoja ja tietoja. Se on myös olennainen osa turvallisuutta ja virheenkäsittelyä.

## Miten tarkistaa, onko hakemistoa olemassa

Hakemistojen olemassaolon tarkistamiseen on useita tapoja, mutta yksi yleisimmistä on käyttää PHP:n `is_dir()` -funktiota. Tämä funktio tarkistaa, onko annettu polku hakemistoksi. Alla on esimerkki koodista, jossa tarkistetaan, onko "kuvat" nimistä hakemistoa olemassa:

```PHP
if (is_dir("kuvat")) {
  echo "Hakemisto löytyy!";
} else {
  echo "Hakemistoa ei löydy.";
}
```

Jos "kuvat" nimistä hakemistoa ei olisi olemassa, tulostettaisiin "Hakemistoa ei löydy." Voit myös käyttää `is_dir()` -funktiota yhdessä `echo`-komennon kanssa tulostamaan hakemiston nimen:

```PHP
if (is_dir("kuvat")) {
  echo "Hakemisto on 'kuvat'.";
} else {
  echo "Hakemistoa ei löydy.";
}
```

Tämä tulostaisi "Hakemisto on 'kuvat'." Jos haluat tarkistaa, onko hakemisto olemassa jossain muussa sijainnissa, voit antaa `is_dir()` -funktiolle absoluuttisen polun:

```PHP
if (is_dir("/var/www/html/kuvat")) {
  echo "Hakemisto löytyy sijainnista /var/www/html/kuvat";
} else {
  echo "Hakemistoa ei löydy.";
}
```
Tässä esimerkissä tarkistetaan, löytyykö hakemisto "kuvat" sijainnista "/var/www/html".

## Syvempi sukellus

Kun PHP tarkistaa hakemistojen olemassaoloa `is_dir()` -funktiolla, se etsii fyysisesti tiedostojärjestelmästä hakemistoa, jonka polku on annettu funktion parametrina. Siksi on tärkeää antaa oikea polku, jotta tarkistaminen toimii oikein.

Voit myös käyttää `!` (NOT) -operaattoria tarkistamaan, ettei hakemistoa ole olemassa:

```PHP
if (!is_dir("kuvat")) {
  echo "Hakemistoa ei löydy.";
}
```

## Katso myös

- PHP:n `file_exists()` -funktio https://www.php.net/manual/en/function.file-exists.php
- PHP:n `mkdir()` -funktio https://www.php.net/manual/en/function.mkdir.php