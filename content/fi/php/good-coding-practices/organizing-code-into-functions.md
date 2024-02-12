---
title:                "Koodin järjestäminen funktioihin"
aliases:
- /fi/php/organizing-code-into-functions/
date:                  2024-01-26T01:12:01.663197-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Koodin järjestäminen funktioihin tarkoittaa koodin jaottelua uudelleenkäytettäviksi lohkoiksi määriteltyine tarkoituksineen. Teemme niin pitääksemme asiat järjestyksessä, välttääksemme päällekkäisyyksiä ja tehdäksemme virheenjäljityksen helpoksi.

## Kuinka:
Kuvitellaan, että meillä on toistuvaa koodia käyttäjien tervehtimiseksi. Sen sijaan käärimme sen `greet_user`-nimiseen funktioon:

```php
function greet_user($name) {
    return "Hello, " . $name . "!";
}

echo greet_user("Alice");
echo greet_user("Bob");
```

Tuloste:
```
Hello, Alice!
Hello, Bob!
```

Nyt sinulla on kätevä työkalu, jota voit käyttää milloin tahansa kirjoittamatta samoja koodirivejä uudelleen aina, kun haluat sanoa hei.

## Syväsukellus
Funktiot ovat olleet ohjelmoinnissa mukana jo FORTRANin ajoista 50-luvulta lähtien. Ne ovat rakenteellisen ohjelmoinnin peruskiviä ja kaikki kääntyvät modulaarisuuden ja eristämisen ympärille. Vaihtoehtoja? No, voit siirtyä oliopohjaiseen ohjelmointiin ja puhua luokista ja metodeista, jotka ovat funktioita hienommissa vaatteissa. PHP:n kohdalla toteutuksen yksityiskohdat sisältävät oletusarvojen määrittämisen parametreille, tyypin vihjaamisen syötteille ja mahdollisuuden palauttaa useita arvoja käyttäen taulukkoa tai, PHP 7.1:stä lähtien, listaa.

Tässä on moderni väännös tyypin julistamisen ja oletusarvojen kanssa:

```php
function add(float $a, float $b = 0.0): float {
    return $a + $b;
}

echo add(1.5);
echo add(1.5, 2.5);
```

PHP 7.4 toi mukanaan myös nuolifunktiot, jotka auttavat kirjoittamaan lyhyitä yhden rivin funktioita, joita käytetään yleisesti taulukoiden operaatioissa:

```php
$numbers = array(1, 2, 3, 4);
$squared = array_map(fn($n) => $n * $n, $numbers);
print_r($squared);
```

Tuloste:
```
Array
(
    [0] => 1
    [1] => 4
    [2] => 9
    [3] => 16
)
```

## Katso myös
- [PHP:n käsikirja funktioista](https://www.php.net/manual/en/functions.user-defined.php)
- [PHP: Oikea tapa - Funktiot](https://phptherightway.com/#functions)
- [Lue lisää PHP 7.4:n nuolifunktioista](https://stitcher.io/blog/short-closures-in-php)
