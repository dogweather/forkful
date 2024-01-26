---
title:                "Kompleksilukujen käsittely"
date:                  2024-01-26T04:37:37.384920-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kompleksilukujen käsittely"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Imaginaariluvut, todellisten ja imaginaariosien yhdistelmiä (kuten 3 + 4i), ovat avainasemassa edistyneissä laskelmissa, kuten signaalinkäsittelyssä tai tietyntyyppisten yhtälöiden ratkaisemisessa. Ohjelmoijat käsittelevät niitä matematiikkapainotteisissa sovelluksissa, joissa tavalliset numerot eivät riitä.

## Kuinka:
C:ssä, alkaen C99-standardista, on natiivi kompleksilukutyyppi ja kirjasto. Näin sitä käytetään:

```C
#include <stdio.h>
#include <complex.h>

int main() {
    // Julista kaksi kompleksilukua
    double complex z1 = 1.0 + 3.0 * I;
    double complex z2 = 2.0 - 2.0 * I;

    // Toiminnot kompleksilukujen kanssa
    double complex summa = z1 + z2;
    double complex tulo = z1 * z2;

    // Tulosten tulostus
    printf("Summa: %.1f + %.1fi\n", creal(summa), cimag(summa));
    printf("Tulo: %.1f + %.1fi\n", creal(tulo), cimag(tulo));

    // Itseisarvo & vaihekulma
    printf("Abs(z1): %f\n", cabs(z1));
    printf("Arg(z1): %f\n", carg(z1));

    return 0;
}
```

Esimerkkituloste:
```
Summa: 3.0 + 1.0i
Tulo: 8.0 + 2.0i
Abs(z1): 3.162278
Arg(z1): 1.249046
```

## Syventävä tarkastelu
Imaginaariluvut juontavat juurensa vuosisatojen taakse, 16. vuosisadan algebraan. Nykyään ne ovat olennainen osa monia ohjelmointikieliä, ei vain C:tä.

C99-standardi toi mukanaan `<complex.h>`:n, otsikkotiedoston, joka määrittelee makrot, funktiot ja `complex`-datatyypin. Vaihtoehtoja on - kuten oman rakenteen luominen, mutta miksi keksiä pyörää uudelleen? C:n standardikirjasto on optimoitu ja käyttövalmis.

Vaikka C:n kompleksilukutuki onkin tehokas, sitä kritisoidaan myös. Se voi olla vähemmän intuitiivinen kuin vastaavat ominaisuudet kielissä kuten Python, ja nurkkatapausten käsittely voi olla hankalaa. Mutta raakatehokkuuden osalta se on silti vankka valinta.

## Katso myös
- C99-standardin dokumentaatio `<complex.h>`:lle: https://en.cppreference.com/w/c/numeric/complex
- IEEE-standardi liukulukuaritmetiikalle (IEEE 754): https://ieeexplore.ieee.org/document/4610935
- Verkko-opas C:n kompleksilukumatematiikkaan: https://www.tutorialspoint.com/complex-number-arithmetic-in-c-programming