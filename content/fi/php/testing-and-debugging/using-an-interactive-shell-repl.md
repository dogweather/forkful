---
date: 2024-01-26 04:16:26.575999-07:00
description: "Interaktiivinen kuori, tai REPL (Read-Eval-Print Loop), mahdollistaa\
  \ PHP-koodin kirjoittamisen ja suorittamisen lennosta. Se on ihanteellinen kokeiluihin,\u2026"
lastmod: '2024-03-13T22:44:56.657512-06:00'
model: gpt-4-0125-preview
summary: Interaktiivinen kuori, tai REPL (Read-Eval-Print Loop), mahdollistaa PHP-koodin
  kirjoittamisen ja suorittamisen lennosta.
title: "Interaktiivisen komentotulkin (REPL) k\xE4ytt\xF6"
weight: 34
---

## Mikä & Miksi?
Interaktiivinen kuori, tai REPL (Read-Eval-Print Loop), mahdollistaa PHP-koodin kirjoittamisen ja suorittamisen lennosta. Se on ihanteellinen kokeiluihin, debuggaukseen tai oppimiseen, sillä voit testata koodinpätkiä ilman täyden skriptin luomisen taakkaa.

## Kuinka:
Käynnistä PHP:n REPL suorittamalla `php -a` terminaalissasi. Tässä maistiainen siitä, miten se toimii:

```php
php > echo "Hello, World!";
Hello, World!
php > $arr = [1, 2, 3];
php > print_r($arr);
Array
(
    [0] => 1
    [1] => 2
    [2] => 3
)
```

Voit myös määritellä funktioita:

```php
php > function sum($a, $b) { return $a + $b; }
php > echo sum(5, 10);
15
```

## Syväsukellus
REPL:t ovat olleet olemassa joissain muodoissa alkaen LISP:n varhaisista päivistä 1960-luvulla. PHP:n interaktiivinen kuori on vähemmän kehittynyt verrattuna kielten, kuten Pythonin tai JavaScriptin, vastaaviin. Se ei säilytä tilaa istuntojen välillä ja kaipaa ominaisuuksia, kuten automaattista täydennystä. Monipuolisempaa PHP:n REPL:ää harkitessa kannattaa tutustua vaihtoehtoisiin kuten `psysh` tai `boris`. Nämä kolmannen osapuolen kuoret tarjoavat parempia introspektiotyökaluja, välilehdellä täydennystä ja jopa debuggerin.

PHP:n REPL:n moottorin alla se toimii kääntämällä ja suorittamalla jokaisen koodirivin sitä mukaa kun se syötetään. Tämän lähestymistavan rajoitukset tulevat ilmi esimerkiksi luokkien uudelleen määrittelyssä, mikä ei ole mahdollista samassa istunnossa. Se on hieno yksinkertaisiin testeihin, mutta voi muuttua hankalaksi monimutkaisissa tehtävissä.

## Katso Myös
- [PHP:n Käsikirja - Interaktiivinen kuori](https://www.php.net/manual/en/features.commandline.interactive.php)
- [PsySH: Runtime kehittäjäkonsoli, interaktiivinen debuggeri ja REPL PHP:lle](https://psysh.org/)
- [Boris: Pieni REPL PHP:lle](https://github.com/borisrepl/boris)
