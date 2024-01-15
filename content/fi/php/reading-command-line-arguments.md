---
title:                "Komentoriviparametrien lukeminen"
html_title:           "PHP: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi

"Saatat ihmetellä, mitä hyötyä on lukea komentorivin argumentteja PHP-ohjelmoinnissa. Tässä artikkelissa kerromme, miten ja miksi voit hyödyntää tätä ominaisuutta omassa koodissasi."

## Kuinka tehdä

Jos haluat lukea komentorivin argumentteja PHP-ohjelmassa, sinun tulee ensin käyttää `$_SERVER`-muuttujaa, joka sisältää kaikki komentorivin argumentit. Voit myös hyödyntää `argc`-muuttujaa, joka kertoo, kuinka monta argumenttia on annettu. Tässä yksinkertainen esimerkki:

```PHP 
<?php 
// Hae argumentit 
$args = $_SERVER['argv']; 

// Tulosta kaikki argumentit 
foreach ($args as $arg) { 
    echo $arg . "\n"; 
} 

// Tulosta argumenttien määrä 
echo "Argumentteja annettu: " . $argc; 
?> 
```

Tämän esimerkin suorittaminen komentorivillä antaa seuraavanlaisen outputin:

```
php esimerkki.php arg1 arg2 arg3
arg1
arg2
arg3
Argumentteja annettu: 4
```

## Syvemmälle aiheeseen

Kommentorivin argumentit voivat olla hyödyllisiä esimerkiksi kun haluat käsitellä tiedostoja tai välittää tietoa ohjelmalle suoraan komentoriviltä. Voit myös käyttää `getopt()`-funktiota, joka helpottaa komentorivin argumenttien käsittelyä. Komentorivin argumentteja voi myös muokata ennen niiden käyttöä muuttujina.

## Katso myös

- [PHP `$_SERVER` -dokumentaatio](https://www.php.net/manual/en/reserved.variables.server.php)
- [PHP `getopt()` -dokumentaatio](https://www.php.net/manual/en/function.getopt.php)
- [PHP-komentoriviparametrit -blogikirjoitus](https://www.cloudways.com/blog/command-line-arguments-in-php/)