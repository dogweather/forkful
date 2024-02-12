---
title:                "Numerojen pyöristäminen"
aliases:
- /fi/php/rounding-numbers.md
date:                  2024-01-26T03:45:52.393156-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Numeroiden pyöristäminen tarkoittaa desimaalien poistamista tiettyyn tarkkuuteen, usein kokonaislukuihin. Ohjelmoijat pyöristävät yksinkertaistaakseen laskelmia, parantaakseen suorituskykyä tai tehdäkseen tulosteista käyttäjäystävällisiä.

## Kuinka:
PHP tarjoaa muutaman tavan numeroiden pyöristämiseen: `round()`, `ceil()` ja `floor()`. Näin ne toimivat:

```php
echo round(3.14159);   // Palauttaa 3
echo round(3.14159, 2); // Palauttaa 3.14

echo ceil(3.14159);    // Palauttaa 4, pyöristää aina ylöspäin

echo floor(3.14159);   // Palauttaa 3, pyöristää aina alaspäin
```

## Syväsukellus
Numeroiden pyöristäminen on ollut olennainen osa matematiikkaa ja laskentaa muinaisista ajoista lähtien käsiteltäessä käytännössä mahdottomia äärettömiä desimaaleja. PHP:ssä `round()` voi ottaa tarkkuusparametrin ja tilan, mikä vaikuttaa sen käyttäytymiseen – `PHP_ROUND_HALF_UP`, `PHP_ROUND_HALF_DOWN` yms. määrittelevät, miten se käyttäytyy kohdatessaan ".5" tilanteen. Tarkkuus on avainasemassa taloudellisissa sovelluksissa, joissa pyöristäminen saattaa olla laillisesti säädeltyä, mikä vaikuttaa siihen, miten `round()` toteutetaan koodissa.

Vaihtoehtoja sisäänrakennetuille funktioille ovat mukautetut pyöristysmenetelmät tai BC Math -funktiot mielivaltaisen tarkkuuden aritmetiikkaan, jotka ovat hyödyllisiä skenaarioissa tarvittaessa enemmän hallintaa tai käsiteltäessä hyvin suuria numeroita, joissa natiivi tarkkuus saattaa horjua.

## Katso myös
Tutustu lisää PHP:n käsikirjassa:
- [PHP `ceil` -funktio](https://php.net/manual/en/function.ceil.php)
- [PHP `floor` -funktio](https://php.net/manual/en/function.floor.php)
- [BC Math mielivaltaisen tarkkuuden aritmetiikka](https://php.net/manual/en/book.bc.php)
