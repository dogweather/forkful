---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Bash: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lukea Komennonriviparametreja PHP:ssä Työkalut ja Vinkit

## Mitä & Miksi?
Komennonriviparametrien lukeminen tarkoittaa, että ohjelman suorittamisen aikana annetaan joukko arvoja tai vaihtoehtoja. Tätä tehdään, jotta ohjelmaa voitaisiin mukauttaa tai ohjata tiettyjen tarpeiden mukaisesti jokaisen suorituksen yhteydessä.

## Kuinka toimii:
PHP:n komennonriviparametreja voi lukea `$argv` ja `$argc` globaalin muuttujien avulla. `$argv` on taulukko joka sisältää kaikki parametrit ja `$argc` sisältää parametrien lukumäärän.

```PHP
<?php
// Tulosta kaikki parametrit
print_r($argv);

// Tulosta parametrien määrä
echo "Parametrien määrä: " . $argc;
?>
```

Esimerkissä, jos suoritat skriptin käyttäen komentoa `php script.php param1 param2`, tulostus olisi:

```PHP
Array
(
    [0] => script.php
    [1] => param1
    [2] => param2
)
Parametrien määrä: 3
```
## Syväsukellus
Komennonriviparametrien lukemista on käytetty pitkään ja se on peräisin varhaisista ohjelmointikielistä. Vaihtoehtoisia tapoja sisältävät `getopt`-funktion käytön, joka tarjoaa enemmän joustavuutta ja mahdollistaa vaihtoehtojen määrittämisen.

Yksityiskohtaisemmin, `$argv[0]` sisältää aina skriptin nimen, ja parametrit alkavat indeksistä 1. Muista, että `$argc` sisältää myös skriptinimen, joten se on aina vähintään 1.

## Katso Myös
- PHP.net:n dokumentaatio komentoriviohjelmoinnista: [php.net/manual/en/features.commandline.php](https://php.net/manual/en/features.commandline.php)
- Hyödyllisiä vinkkejä ja jippoja komentoriviparametreihin: [www.sitepoint.com/php-command-line-1/](https://www.sitepoint.com/php-command-line-1/)
- Kattava opas `getopt` funktio: [php.net/manual/en/function.getopt.php](https://www.php.net/manual/en/function.getopt.php)