---
title:                "Numerojen pyöristäminen"
date:                  2024-01-26T03:43:43.655924-07:00
model:                 gpt-4-0125-preview
simple_title:         "Numerojen pyöristäminen"

category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/rounding-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Numeroiden pyöristäminen on desimaalien katkaisemista tietyssä kohdassa, samalla mahdollisesti viimeistä säilytettyä numeroa säätäen. Ohjelmoijat pyöristävät tarkkuutta pienentääkseen, kun tarkat arvot eivät ole tarpeellisia, hallitakseen liukulukuvirheitä tai valmistellakseen numeroita käyttäjäystävälliseen esitysmuotoon.

## Kuinka:
C:ssä tyypillisesti käyttäisit `floor()`, `ceil()` tai `round()` funktioita. Tässä nopea esittely:

```C
#include <stdio.h>
#include <math.h>

int main() {
    double num = 3.14159;
    double num_floor = floor(num);
    double num_ceil = ceil(num);
    double num_round = round(num);

    printf("Floor: %.2f\n", num_floor); // Floor: 3.00
    printf("Ceil: %.2f\n", num_ceil);   // Ceil: 4.00
    printf("Round: %.2f\n", num_round); // Round: 3.00
    return 0;
}
```

Tarkemman kontrollin saamiseksi, kuten pyöristäminen tiettyyn desimaalipaikkaan, kerrot, pyöristät ja jaat:

```C
double roundToPlace(double num, int place) {
    double scale = pow(10.0, place);
    return round(num * scale) / scale;
}

// ...

double num = 3.14159;
double num_rounded = roundToPlace(num, 2);
printf("Pyöristetty 2 desimaalin tarkkuuteen: %.2f\n", num_rounded); // Pyöristetty 2 desimaalin tarkkuuteen: 3.14
```

## Syväsukellus
Aikanaan, pyöristäminen tarkoitti usein manuaalista prosessia – raskas tehtävä kynällä ja paperilla. Tietokoneiden myötä automatisoimme tämän, mutta liukulukuaritmeetiikka toi mukanaan nyansseja sen binääriluonteen vuoksi, jossa jotkin numerot eivät voi olla täsmällisesti esitettävissä.

Vaihtoehtoja standardipyöristykselle sisältävät katkaisun (ylimääräisten numeroiden yksinkertaisen pudottamisen) tai pankkiirin pyöristyksen, joka pyöristää lähimpään parilliseen numeroon, kun ollaan tarkasti kahden arvon välissä, vähentäen vinoumaa toistuvissa laskelmissa.

Toteutus muuttuu haastavaksi, kun on tarve pyöristää mielivaltaisen tarkkuuden numeroita tai käsitellä erikoistapauksia kuten äärettömyydet, signaloivat NaN-arvot tai subnormaalit arvot. C:n standardikirjaston funktiot käsittelevät perusteet, mutta jos tarvitset pyöristää desimaaleja erikoisilla tavoilla, tarvitset enemmän kuin `math.h`.

## Katso myös
- [`<math.h>` dokumentaatio](https://en.cppreference.com/w/c/numeric/math)
- [Liukulukulaskentojen varmentamisen ansoja](https://dl.acm.org/doi/10.1145/1186736.1186737)
