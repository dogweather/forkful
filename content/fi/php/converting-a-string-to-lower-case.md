---
title:                "PHP: Merkkijonon muuntaminen pienaakkosiksi"
programming_language: "PHP"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/php/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi: 
Voit olla tilanteessa, jossa sinun täytyy muuttaa merkkijono pienaakkosiin PHP-ohjelmassa. Se voi olla tarpeellista esimerkiksi vertaillessasi kahta merkkijonoa, jotta varmistat niiden samanmuotoisuuden. Tässä blogikirjoituksessa opit, miten voit helposti konvertoida merkkijonon pienaakkosiin PHP:llä.

## Miten: 
Käytännössä, PHP:llä on valmiina funktio, jolla voidaan konvertoida merkkijono pienaakkosiin. Tämä funktio on nimeltään strtolower(). Alla on esimerkkikoodi, jossa käytämme tätä funktiota ja tulostamme muutetun merkkijonon:

```PHP
<?php
$string = "TÄMÄ ON MERKKIJONO";
echo strtolower($string);
// Output: tämä on merkkijono
?>
```

Kuten näet, funktio muutti kaikki isot kirjaimet pieniksi kirjaimiksi. Tämä toimii myös erikoismerkkien kanssa, kuten skandinaavisilla kirjaimilla.

## Syvällisempi tarkastelu: 
Jos haluat tarkemmin tietoa siitä, miten PHP konvertoi merkkijonon pienaakkosiin, niin tässä on pieni syvällisempi tarkastelu. PHP:ssä käytettävät isot ja pienet kirjaimet perustuvat ASCII-taulukkoon. Jokaisella kirjaimella on oma numeerinen arvo ja isojen ja pienten kirjainten välillä on 32 numeron ero. Tämä tarkoittaa sitä, että jos haluamme muuttaa ison kirjaimen pieneksi, niin meidän täytyy lisätä sen numeeriseen arvoon 32.

Kun PHP kohdataan stringin, se käy merkki kerrallaan läpi ja tarkistaa sen numeerisen arvon. Jos kyseessä on iso kirjain, niin PHP lisää siihen 32 ja muuttaa sen pieneksi kirjaimeksi.

## Katso myös: 
- PHP manuaali tästä aiheesta
- Tietoa ASCII-taulukosta
- Muita hyödyllisiä PHP-funktioita merkkijonoille