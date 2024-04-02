---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:34:18.358640-07:00
description: "Virheviestien kirjoittaminen standardivirheeseen (stderr) PHP:ss\xE4\
  \ tarkoittaa virheilmoitusten tai diagnostiikkatietojen ohjaamista erill\xE4\xE4\
  n\u2026"
lastmod: '2024-03-13T22:44:56.672149-06:00'
model: gpt-4-0125-preview
summary: "Virheviestien kirjoittaminen standardivirheeseen (stderr) PHP:ss\xE4 tarkoittaa\
  \ virheilmoitusten tai diagnostiikkatietojen ohjaamista erill\xE4\xE4n\u2026"
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Mikä ja miksi?

Virheviestien kirjoittaminen standardivirheeseen (stderr) PHP:ssä tarkoittaa virheilmoitusten tai diagnostiikkatietojen ohjaamista erillään standarditulosteesta (stdout), mikä antaa kehittäjille paremmat mahdollisuudet hallita tulostevirtojaan debuggauksen ja lokitiedostojen kannalta. Ohjelmoijat käyttävät tätä tekniikkaa varmistaakseen, että virhesanomat eivät sekaannu ohjelman tulosteisiin, mikä helpottaa sovellusten seurantaa ja vianetsintää.

## Kuinka:

PHP:ssä stderr:iin kirjoittaminen onnistuu käyttämällä `fwrite()`-funktiota yhdessä ennalta määrätyn vakion `STDERR` kanssa, joka edustaa virheen tulostevirtaa.

```php
<?php
// Yksinkertaisen sanoman kirjoittaminen stderr:iin.
fwrite(STDERR, "Tämä on virhesanoma.\n");
```

Esimerkkitulo, kun skripti suoritetaan komentoriviltä:
```
Tämä on virhesanoma.
```

Esitelläksemme käytännöllisempää käyttöä, kuvittele skenaario, jossa käsittelet käyttäjän syötettä ja kohtaat odottamattoman datan:
```php
<?php
$input = 'odottamaton data';

// Simuloimassa virhettä käyttäjän syötteen käsittelyssä.
if ($input === 'odottamaton data') {
    fwrite(STDERR, "Virhe: Odottamatonta syötettä saatu.\n");
    exit(1); // Poistutaan ei-nolla-arvolla osoittaen virhetilaa.
}
```

Vaikka PHP:n sisäänrakennetut toiminnot stderr:n käsittelyyn ovat yleensä riittäviä, kun käsitellään monimutkaisempia sovelluksia tai halutaan integroida stderr-lokitus ulkoisiin järjestelmiin, kolmannen osapuolen kirjastot kuten Monolog voivat olla voimakas liittolainen. Monolog on lokituskirjasto, joka pystyy käsittelemään stderr:iä monien muiden kohteiden (tiedostot, socketit jne.) ohella.

Käyttäen Monologia kirjoittaaksesi stderr:iin:

Varmista ensin, että sinulla on Monolog asennettuna Composerin kautta:
```
composer require monolog/monolog
```

Sitten voit konfiguroida Monologin käyttämään `StreamHandler`-käsittelijää kohteena `php://stderr`:

```php
<?php
require 'vendor/autoload.php';

use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Luo lokituskanava
$log = new Logger('nimi');
$log->pushHandler(new StreamHandler('php://stderr', Logger::WARNING));

// Lisää lokiviesti stderr:iin
$log->warning('Tämä on varoitusviesti.');
```

Yllä oleva koodi käyttää Monologia lähettääkseen varoitusviestin stderr:iin, mikä on erityisen hyödyllistä sovelluksille, jotka vaativat yksityiskohtaisia lokitusasetuksia tai ulkoista lokien seurantaa.
