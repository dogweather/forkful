---
title:    "Fish Shell: Satunnaislukujen luominen"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Halusitko koskaan luoda pelin, jossa on satunnaisia elementtejä? Tai kenties laskentaprojektissa tarvitset satunnaista dataa? Fish Shell tarjoaa helpon tavan luoda satunnaisia numeroita, joka on hyödyllinen monissa ohjelmointitilanteissa.

## Miten

Fish Shell:lla on valmiiksi sisäänrakennettu komento, `math random`, jolla voidaan generoida satunnaislukuja. Se ottaa parametrina minimi- ja maksimiarvon, ja palauttaa satunnaisen luvun niiden väliltä.

```Fish Shell
math random 1 100
```

Tämä esimerkki palauttaisi satunnaisen kokonaisluvun väliltä 1-100, esimerkiksi 78. Voit myös käyttää muita muuttujia, kuten `echo`, tulostamaan satunnaislukuja.

```Fish Shell
set luku (math random 1 10)
echo "Satunnainen luku on $luku"
```

Tämä tulostaisi esimerkiksi "Satunnainen luku on 6".

## Syvään sukeltaminen

Fish Shellin `math random` komento käyttää Perl-kirjastoa satunnaislukujen generoimiseen. Tarkempien ohjeiden ja vaihtoehtoisten parametrien löytämiseksi voit katsoa Fish Shellin dokumentaatiota tai Perl-kirjaston dokumentaatiota.

## Katso myös

- [Fish Shell:nnen dokumentaatio](https://fishshell.com/docs/current/cmds/math.html)
- [Perl-kirjaston dokumentaatio](https://perldoc.perl.org/functions/rand.html)