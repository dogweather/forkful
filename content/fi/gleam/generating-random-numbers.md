---
title:    "Gleam: Satunnaislukujen luominen"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## Miksi

Usein ohjelmointitehtävissä tarvitaan satunnaislukuja, kuten arpajaisissa tai pelien simulaatioissa. Gleam tarjoaa helpon ja luotettavan tavan generoida satunnaislukuja, joten programmoijana sinun ei tarvitse huolehtia monimutkaisista laskukaavoista tai turvallisuusriskeistä.

## Miten

Gleamissa satunnaislukuja generoidaan `random` moduulin avulla. Ensimmäiseksi täytyy tuoda moduuli käyttöön koodissa:

```Gleam
import random
```

Sitten voidaan kutsua `random.int` funktiota, joka ottaa kaksi parametria: `min` ja `max`, ja palauttaa satunnaisen kokonaisluvun niiden väliltä.

```Gleam
let n = random.int(1, 10)
```

Tämä koodi generoi satunnaisen numeron väliltä 1 ja 10, ja tallentaa sen `n` muuttujaan. Voit muokata `min` ja `max` parametreja tarvittaessa. 

Gleamissa on myös muita hyödyllisiä funktioita kuten `random.float` ja `random.string`, jotka toimivat samalla periaatteella kuin `random.int`.

## Syventyminen

Gleam käyttää Mersenne Twister -algoritmia satunnaislukujen generoimiseen, joka on yksi parhaiten tunnetuista ja testatuimmista satunnaislukualgoritmeista. Tämän ansiosta voit luottaa siihen, että Gleam generaattorit ovat luotettavia ja tuottavat tasaisesti jakautuneita lukuja.

On myös hyvä huomioida, että Gleamin satunnaislukujen generaattori käyttää aina samaa siemenarvoa koodin suorituksessa, joten jos haluat eri satunnaisia lukuja joka kerta, kannattaa käyttää jotain ulkoista arvoa kuten aikaleimaa tai käyttäjän antamaa syötettä siemenarvona.

## Katso myös

- [Gleamin virallinen dokumentaatio satunnaislukujen generoinnista](https://gleam.run/documentation/guides/random#overview)
- [Mersenne Twister algoritmin selitys (englanniksi) ](https://en.wikipedia.org/wiki/Mersenne_Twister)