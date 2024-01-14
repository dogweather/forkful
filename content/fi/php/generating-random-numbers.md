---
title:    "PHP: Sattumalukujen luominen"
keywords: ["PHP"]
---

{{< edit_this_page >}}

## Miksi
Monet ohjelmat ja sovellukset käyttävät satunnaisia numeroita eri tarkoituksiin, kuten salaus, arpajaiset tai pelin toiminta. PHP tarjoaa useita tapoja generoida satunnaisia lukuja, joiden avulla voit lisätä ennustettavuutta ja monipuolisuutta ohjelmointisi. Tämä blogikirjoitus antaa sinulle yleiskatsauksen satunnaislukujen generointiin PHP:ssa ja selittää, miksi se voi olla hyödyllistä.

## How To
PHP:ssa on sisäänrakennettu funktio nimeltä `rand()`, jolla voit generoida satunnaisen kokonaisluvun annetulta väliltä. Esimerkiksi, jos haluat generoida satunnaisen luvun väliltä 1-10, käytä `rand(1,10)`. Voit myös käyttää `mt_rand()`-funktiota, joka käyttää paremmin jakautuvaa satunnaislukugeneraattoria ja voi tuottaa tarkempia tuloksia. Käyttötapauksesi mukaan valitse sopivin vaihtoehto.

Jos haluat generoida satunnaisen liukuluvun, voit käyttää `mt_rand()`-funktiota yhdessä `$_GET`-muuttujan kanssa, joka tarjoaa satunnaisen numeron väliltä 0-1. Jos haluat generoida satunnaisen liukuluvun tietyltä väliltä, voit käyttää laskutoimituksia tai `rand()`-funktiota muuttamaan lukujen välit.

```PHP
// Generoi satunnaisen kokonaisluvun väliltä 1-10
rand(1,10);

// Generoi satunnaisen liukuluvun väliltä 0-1
mt_rand(0,1);

// Generoi satunnaisen liukuluvun väliltä 0-100
mt_rand(0,1) * 100;
```

PHP:ssa on myös mahdollista generoida satunnainen merkkijono `uniqid()`-funktion avulla. Voit antaa halutun pituuden ja lisätä haluamiasi merkkejä alkuun tai loppuun.

```PHP
// Generoi satunnainen merkkijono
uniqid();

// Generoi satunnainen merkkijono, jonka pituus on 10 ja joka alkaa 'ID'
"ID" . uniqid('', true);
```

## Deep Dive
PHP käyttää pseudosatunnaislukugeneraattoria, joka tuottaa sarjan lukuja peräkkäin perustuen yhteen alkulukuun. Näitä lukuja ei voida kutsua täysin satunnaisiksi, mutta niillä on hyvä taso ennalta-arvaamattomuutta.

Jos haluat lisätä ennustettavuutta satunnaislukujen generointiin, voit käyttää `srand()`-funktiota asettaaksesi siemennumeron. Tämä mahdollistaa tiettyjen satunnaislukujen toistumisen.

Voit myös käyttää `random_int()`-funktiota, joka on PHP 7:ssä ja uudemmissa versioissa. Tämä funktio käyttää kryptografista satunnaislukugeneraattoria, joka on turvallisempi arkaluonteisiin tietoihin.

## Katso myös
- [PHP-Random-funktio](https://www.php.net/manual/en/function.rand.php)
- [PHP-Math-funktio](https://www.php.net/manual/en/ref.math.php)
- [PHP-Sanitization-funktio](https://www.php.net/manual/en/ref.filter.php)
- [PHP-7 boa-artikkeli "Random Generators"](https://www.php.net/manual/en/..random-generators.php)