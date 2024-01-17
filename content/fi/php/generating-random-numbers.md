---
title:                "Satunnaislukujen luominen"
html_title:           "PHP: Satunnaislukujen luominen"
simple_title:         "Satunnaislukujen luominen"
programming_language: "PHP"
category:             "PHP"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Satunnaislukujen generointi on prosessi, jossa tietokone luo lukuja, jotka eivät noudata mitään tietyä kaavaa tai järjestystä. Ohjelmoijat käyttävät satunnaislukugenerointia moniin eri tarkoituksiin, kuten pelien luomiseen, salauksen algoritmien luomiseen ja testitietojen luomiseen.

## Kuinka:

```PHP 
echo rand(); 
``` 
Tämä yksinkertainen koodinpätkä generoi satunnaisen kokonaisluvun ja tulostaa sen ruudulle.

```PHP 
echo rand(1, 10); 
``` 
Tässä esimerkissä satunnainen luku generoidaan välillä 1-10.

## Syvennys:

Satunnaislukugenerointia on käytetty tietokoneohjelmoinnissa jo vuosikymmenten ajan. Alkuperäinen algoritmi, jota käytettiin satunnaislukujen generointiin, käytti tietokoneen kelloa lähteenä. Nykyään useimmat ohjelmointikielet, kuten PHP, sisältävät sisäänrakennetun funktion satunnaislukujen generointiin.

On myös olemassa muita tapoja luoda satunnaislukuja ohjelmoinnissa. Yksi esimerkki on niin kutsuttu pseudorandom-toiminto, jossa luvut eivät ole täysin satunnaisia, vaan ne generoidaan tietyn matemaattisen kaavan avulla.

Satunnaislukujen generointiin liittyy myös useita tärkeitä käsitteitä, kuten seed-arvo, joka määrittää mistä kohtaa aloitetaan lukuja generoitaessa, ja pseudo-satunnainen sekvenssi, jossa sama seed-arvo tuottaa saman sarjan satunnaislukuja.

## Katso myös:

PHP:n virallinen dokumentaatio satunnaislukugenerointiin: https://www.php.net/manual/en/function.rand.php

Hyödyllisiä vinkkejä satunnaislukugenerointiin: https://www.geeksforgeeks.org/php-program-to-generate-a-random-number-in-a-given-range/