---
title:    "PHP: Sattumanvaraisten numeroiden generoiminen"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/php/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Satunnaislukujen luominen on tärkeä osa monia ohjelmointitehtäviä, kuten pelejä, salausmenetelmiä ja data-analyysiä. Satunnaislukujen avulla voidaan luoda ennakoimattomia ja monipuolisia tuloksia, jotka tekevät ohjelmista mielenkiintoisempia ja turvallisempia.

## Kuinka

```PHP
// Satunnaisen kokonaisluvun generointi
rand();

// Satunnaisen desimaaliluvun generointi annetulta väliltä
rand(10, 100);

// Satunnaisen merkkijonon generointi määritellyn pituuden ja merkkejoukon avulla
$characters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
$result = substr(str_shuffle($characters), 0, 8);

echo $result; // tulostaa esimerkiksi "oPGeslJd"

// Satunnaisen boolean-arvon generointi
(bool)rand(0, 1); // palauttaa joko true tai false
```

Lopputulos voi vaihdella jokaisella kerralla, kun koodia suoritetaan. Näin saadaan luotua arvaamattomia tuloksia, jotka tekevät ohjelmista mielenkiintoisempia.

## Syvällinen sukellus

PHP:ssä on käytössä useita suuria satunnaislukugeneraattoreita, kuten `mt_rand()` ja `random_int()`, jotka ovat tehokkaampia kuin yksinkertainen `rand()`. Näiden generoimia lukuja pidetään myös turvallisempina, sillä niiden luonteesta tullee vaikeammin arvattavia.

On myös tärkeää huomata, että satunnaislukujen generointiin käytettävät algoritmit ovat matemaattisia ja siten eivät voi olla täysin arvaamattomia. Esimerkiksi `rand()` käyttää standardoimatonta Mersenne Twister -generaattoria, mikä tarkoittaa, että suuri määrä generoituja lukuja voisi potentioida koodin ennustettavuutta.

## Katso myös

- PHP:n virallinen dokumentaatio satunnaislukujen generoinnista: https://www.php.net/manual/en/function.rand.php
- Artikkeli parhaista tavoista generoida satunnaislukuja PHP:ssä: https://stackoverflow.com/questions/550023/generate-a-random-number-within-range/54695033#54695033