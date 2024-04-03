---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:25.662840-07:00
description: "Kuinka: PHP:ss\xE4 hakemiston olemassaolon voi tarkistaa natiivisti\
  \ k\xE4ytt\xE4m\xE4ll\xE4 `is_dir()`-funktiota. T\xE4m\xE4 funktio ottaa argumentiksi\
  \ tiedostopolkun ja\u2026"
lastmod: '2024-03-13T22:44:56.670603-06:00'
model: gpt-4-0125-preview
summary: "PHP:ss\xE4 hakemiston olemassaolon voi tarkistaa natiivisti k\xE4ytt\xE4\
  m\xE4ll\xE4 `is_dir()`-funktiota."
title: Tarkistetaan, onko hakemisto olemassa
weight: 20
---

## Kuinka:
PHP:ssä hakemiston olemassaolon voi tarkistaa natiivisti käyttämällä `is_dir()`-funktiota. Tämä funktio ottaa argumentiksi tiedostopolkun ja palauttaa `true`, jos hakemisto on olemassa ja on hakemisto, tai `false` muussa tapauksessa.

```php
$directoryPath = "/polku/hakemistoosi";

if(is_dir($directoryPath)) {
    echo "Hakemisto on olemassa.";
} else {
    echo "Hakemistoa ei ole olemassa.";
}
```

Esimerkkituloste:
```
Hakemisto on olemassa.
```
Tai, jos hakemistoa ei ole olemassa:
```
Hakemistoa ei ole olemassa.
```

Vaikka PHP:n vakio kirjasto on riittävän kattava useimpiin hakemisto- ja tiedostomanipulointitehtäviin, saatat joskus tarvita kattavampaa ratkaisua. Tällaisissa tapauksissa suosittu kolmannen osapuolen kirjasto on Symfony-tiedostojärjestelmän komponentti. Se tarjoaa laajan valikoiman tiedostojärjestelmätyökaluja, mukaan lukien yksinkertaisen tavan tarkistaa, onko hakemisto olemassa.

Ensimmäisenä sinun täytyy asentaa Symfony-tiedostojärjestelmän komponentti. Jos käytät Composeria (riippuvuuksienhallintajärjestelmä PHP:lle), voit suorittaa seuraavan komennon projektihakemistossasi:

```
composer require symfony/filesystem
```

Symfony-tiedostojärjestelmän komponentin asentamisen jälkeen voit käyttää sitä tarkistaaksesi, onko hakemisto olemassa seuraavasti:

```php
use Symfony\Component\Filesystem\Filesystem;

$filesystem = new Filesystem();
$directoryPath = '/polku/hakemistoosi';

if($filesystem->exists($directoryPath)) {
    echo "Hakemisto on olemassa.";
} else {
    echo "Hakemistoa ei ole olemassa.";
}
```

Esimerkkituloste:
```
Hakemisto on olemassa.
```
Tai, jos hakemistoa ei ole olemassa:
```
Hakemistoa ei ole olemassa.
```

Molemmat menetelmät tarjoavat luotettavat tavat tarkistaa hakemiston olemassaolon PHP:ssä. Valinta PHP:n sisäänrakennettujen funktioiden ja kolmannen osapuolen kirjaston, kuten Symfonyn tiedostojärjestelmäkomponentin, välillä riippuu projektisi erityistarpeista ja siitä, tarvitsetko lisää tiedostojärjestelmän manipulointia, joka saattaisi olla tehokkaammin käsiteltävissä kirjaston avulla.
