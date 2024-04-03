---
date: 2024-01-26 03:45:52.393156-07:00
description: "Kuinka: PHP tarjoaa muutaman tavan numeroiden py\xF6rist\xE4miseen:\
  \ `round()`, `ceil()` ja `floor()`. N\xE4in ne toimivat."
lastmod: '2024-03-13T22:44:56.650659-06:00'
model: gpt-4-0125-preview
summary: "PHP tarjoaa muutaman tavan numeroiden py\xF6rist\xE4miseen."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

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
