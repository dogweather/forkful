---
title:                "Tulevaisuuden tai menneen päivämäärän laskeminen"
html_title:           "C: Tulevaisuuden tai menneen päivämäärän laskeminen"
simple_title:         "Tulevaisuuden tai menneen päivämäärän laskeminen"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi joku haluaisi laskea tulevan tai menneen päivämäärän. Esimerkiksi ohjelmointiprojekteissa voi olla tarvetta laskea päiviä tai vuosia tietystä päivämäärästä eteen- tai taaksepäin. Tässä artikkelissa opit, miten voit helposti laskea päivämääriä käyttämällä C-kieltä.

## Miten

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Määritellään muuttujat päivämääräksi ja vuosiksi
    int paivat, vuodet;
    // Kysytään käyttäjältä haluttu päivämäärä ja tallennetaan se muuttujaan
    printf("Anna haluttu päivämäärä: ");
    scanf("%d", &paivat);

    // Lasketaan annetun päivämäärän ja nykyisen päivämäärän välinen ero päivissä
    int ajassaSiirtyma = paivat - today;
    // Muunnetaan päivät vuosiksi jakamalla ne 365 päivällä
    vuodet = ajassaSiirtyma / 365;

    // Tulostetaan vuosien määrä
    printf("Annettuna päivämääränä on kulunut %d vuotta.", vuodet);

    return 0;
}
```

Esimerkissä käytämme nykyisen päivämäärän arvoa laskemaan annetun päivämäärän ja nykyisen päivämäärän välisen eron päivinä. Tämän jälkeen muutamme päivät vuosiksi jakamalla ne 365 päivällä. Lopuksi tulostamme vuosien määrän käyttäjälle.

## Syventävä sukellus

Jos haluat laskea päivämääriä tarkemmalla tasolla, voit käyttää C-kielen `time.h` -kirjastoa. Tämä kirjasto tarjoaa erilaisia toimintoja, jotka sallivat päivämäärien laskemisen ja muokkaamisen muilla tarkkuustasoilla. Voit esimerkiksi käyttää `struct tm` -rakennetta tallentamaan ja käsittelemään päivämääriä. Lisätietoja tästä löydät C-kielen `time.h` -dokumentaatiosta.

## Katso myös

- https://www.tutorialspoint.com/c_standard_library/time_h.htm
- https://www.cs.utah.edu/~germain/PPS/Topics/time.html
- https://www.programiz.com/c-programming/library-function/time.h