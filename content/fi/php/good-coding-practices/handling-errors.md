---
date: 2024-01-26 00:55:57.245131-07:00
description: "Miten: PHP:ss\xE4 voit hallita virheit\xE4 k\xE4ytt\xE4m\xE4ll\xE4 `try-catch`-lohkoja,\
  \ ja voit mukauttaa prosessia omien virhek\xE4sittelij\xF6iden ja poikkeusten avulla."
lastmod: '2024-03-13T22:44:56.663419-06:00'
model: gpt-4-1106-preview
summary: "PHP:ss\xE4 voit hallita virheit\xE4 k\xE4ytt\xE4m\xE4ll\xE4 `try-catch`-lohkoja,\
  \ ja voit mukauttaa prosessia omien virhek\xE4sittelij\xF6iden ja poikkeusten avulla."
title: "Virheiden k\xE4sittely"
weight: 16
---

## Miten:
PHP:ssä voit hallita virheitä käyttämällä `try-catch`-lohkoja, ja voit mukauttaa prosessia omien virhekäsittelijöiden ja poikkeusten avulla.

```php
// Perus try-catch esimerkki
try {
  // Tee jotain riskialtista
  $file = fopen("olematontiedosto.txt", "r");
} catch (Exception $e) {
  // Käsittele virhe
  echo "Virhe: " . $e->getMessage();
}

// Aseta oma virhekäsittelijä
set_error_handler(function($severity, $message, $file, $line) {
  throw new ErrorException($message, 0, $severity, $file, $line);
});

// Käyttämällä poikkeuksia
class MyException extends Exception {}

try {
  // Tee jotain ja heitä oma poikkeus
  throw new MyException("Mukautettu virhe!");
} catch (MyException $e) {
  // Käsittele mukautettu poikkeus
  echo $e->getMessage();
}

// Esimerkkituloste:
// Virhe: fopen(olematontiedosto.txt): ei onnistu avaamaan virtaa: Tiedostoa tai hakemistoa ei ole
// Mukautettu virhe!
```

## Syväluotaus
Aikaisemmin PHP-virheet olivat enemmän varoituksia ja huomautuksia, jotka eivät pysäyttäneet skriptin suorittamista. Kielen kehittyessä se omaksui robustimman oliopohjaisen virhekäsittelyn Exception-luokan kautta, joka otettiin käyttöön PHP 5:ssä. Myöhemmin PHP 7 toi Error-luokat, jotka viimein erottelivat virheet ja poikkeukset toisistaan.

Ennen `try-catch`-lohkoja PHP käytti `set_error_handler()`-funktiota virheiden käsittelyyn. `try-catch` on siistimpi, modernimpi. Mutta omilla virhekäsittelijöillä on edelleen sijansa, erityisesti vanhan koodin kanssa tai silloin, kun tarvitsee ottaa kiinni sellaiset virheet, jotka eivät normaalisti ole poikkeuksia.

PHP 7+:n `Throwable`-rajapinta tarkoittaa, että olipa kyseessä Error tai Exception, voit nyt ottaa kiinni molemmat. Tämä on kätevää, koska nyt et missaa kriittisiä suoritusaikaisia virheitä, joita oli aiemmin vaikeampi jäljittää.

Vaihtoehtoja PHP:n sisäänrakennetun mekanismin ulkopuolella sisältävät kirjastot ja kehykset, jotka tulevat oman virhekäsittelyjärjestelmän kanssa, tarjoten lisäominaisuuksia kuten virhelokien kirjaamista tiedostoihin tai käyttäjäystävällisten virhesivujen näyttämistä.

## Katso myös
- Virallinen PHP-dokumentaatio poikkeuksista: https://www.php.net/manual/en/language.exceptions.php
- PHP The Right Way virheraportoinnista: https://phptherightway.com/#error_reporting
- PHP-käsikirja virhekäsittelystä: https://www.php.net/manual/en/book.errorfunc.php
