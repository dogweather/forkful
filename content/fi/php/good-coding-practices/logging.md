---
date: 2024-01-26 01:07:19.359731-07:00
description: "Lokittaminen on k\xE4yt\xE4nn\xF6ss\xE4 kuin p\xE4iv\xE4kirjan pit\xE4\
  mist\xE4 koodillesi; se on tapa tallentaa tapahtumia, virheit\xE4 ja muita merkitt\xE4\
  vi\xE4 datapisteit\xE4, jotka\u2026"
lastmod: '2024-03-13T22:44:56.662264-06:00'
model: gpt-4-1106-preview
summary: "Lokittaminen on k\xE4yt\xE4nn\xF6ss\xE4 kuin p\xE4iv\xE4kirjan pit\xE4mist\xE4\
  \ koodillesi; se on tapa tallentaa tapahtumia, virheit\xE4 ja muita merkitt\xE4\
  vi\xE4 datapisteit\xE4, jotka\u2026"
title: Lokitus
---

{{< edit_this_page >}}

## Mikä ja miksi?

Lokittaminen on käytännössä kuin päiväkirjan pitämistä koodillesi; se on tapa tallentaa tapahtumia, virheitä ja muita merkittäviä datapisteitä, jotka tapahtuvat sovelluksesi suorittaessa. Ohjelmoijat tekevät sitä pitääkseen kirjaa siitä, mitä "konepellin alla" tapahtuu, virheiden jäljittämiseen ja auditointikirjanpidon ylläpitämiseen myöhempää analysointia tai vaatimustenmukaisuutta varten.

## Kuinka:

PHP:ssä on sisäänrakennettu virhelokitusfunktio, joka on helppokäyttöinen. Lisää vain `error_log()` koodiisi lähettääksesi viestin palvelimesi lokeihin. Voit myös muokata sitä kirjoittamaan tiettyyn tiedostoon.

```php
<?php
// Yksinkertaisen tiedotusviestin lokittaminen
error_log("Tämä on tiedotuslokin merkintä.");

// Virheviestin lokittaminen
error_log("Tämä on virhelokin merkintä.", 0);

// Lokitus määritettyyn tiedostoon
file_put_contents('/polku/sinun/custom.log', "Muokattu lokimerkintä.\n", FILE_APPEND);

// Structuroitu lokitus Monologin avulla
require 'vendor/autoload.php';
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Luo logger
$logger = new Logger('nimi');
// Lisää nyt muutama käsittelijä
$logger->pushHandler(new StreamHandler('/polku/sinun/monolog.log', Logger::WARNING));

// Nyt voit käyttää loggeriasi
$logger->warning('Tämä on varoituslokin merkintä!');
$logger->error('Tämä on virhelokin merkintä!');
?>
```

Tämä tulostaa lokisi joko palvelimen lokiin tai määrittelemääsi tiedostoon tavallisessa tekstimuodossa.

## Syväsukellus:

Aiemmin PHP-kehittäjät turvautuivat `error_log()`-funktioon tai Apache/Nginx-lokeihin ongelmien havaitsemiseksi, mutta se voi olla kaoottista, kun tarvitsisi jäsentää tavallisia tekstitiedostoja eikä ole helppoa tapaa suodattaa tai järjestää niitä. Tästä syystä lokitus-kirjastot kuten Monolog, ovat tuoneet PHP:hen rakenteellisen lokituksen aikakauden. Nämä ratkaisut tarjoavat paremman hallinnan monipuolisten lokituskanavien, vakavuustasojen ja muotoillun tulosteen (kuten JSON, mikä on unelma ohjelmallisessa jäsentämisessä) avulla.

Vaihtoehtoja Monologille sisältävät Log4php, KLogger ja Apachen Log4php. Toteutukseltaan vankka lokitus vaatii muutakin kuin datan pudottamista minne sattuu, vaan myös asioiden kuten lokikierron, arkistointistrategioiden ja integrointi valvontatyökaluihin ottamista huomioon ollakseen todella hyödyllistä.

Sinun tulisi pitää mielessä [PSR-3 Logger Interface](https://www.php-fig.org/psr/psr-3/), joka hahmottelee yhteisen rajapinnan lokitus-kirjastoille, varmistaen yhteensopivuutta ja johdonmukaista tapaa päästä käsiksi lokitusmekanismeihin.

## Katso myös:

- [Monolog GitHub-varasto](https://github.com/Seldaek/monolog)
- [PSR-3 Logger Interface -määrittely](https://www.php-fig.org/psr/psr-3/)
- [PHP Virhelokidokumentaatio](https://www.php.net/manual/en/function.error-log.php)
- [KLogger: Yksinkertainen lokitusluokka PHP:lle](https://github.com/katzgrau/KLogger)
- [Log4php: Monikäyttöinen lokituskehyssarja PHP:lle](https://logging.apache.org/log4php/)

Kokeile ensin sisäänrakennettuja toimintoja, mutta tavoittele pitkäikäisempää ja skaalautuvampaa lähestymistapaa, harkitse ajan investoimista tutustuaksesi kirjastoon kuten Monolog. Iloisia lokitushetkiä!
