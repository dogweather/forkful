---
title:                "Hankkimassa nykyinen päivämäärä"
html_title:           "C: Hankkimassa nykyinen päivämäärä"
simple_title:         "Hankkimassa nykyinen päivämäärä"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Miksi
Kuinka saada nykyinen päivämäärä C-kielellä? Tässä artikkelissa käymme läpi syyn taustalla ja kuinka se on mahdollista toteuttaa.

## Kuinka tehdä
Nykypäivän C-koodarit usein tarvitsevat käyttää nykyisen päivämäärän tietoa ohjelmissaan. Tämä voidaan suorittaa käyttämällä `time.h` kirjastoa ja sen sisältämää `time` rakennetta. Seuraavassa koodiesimerkissä luodaan muuttuja `current_time`, joka sisältää nykyisen päivämäärän tiedot.

```C 
#include <stdio.h>
#include <time.h>

int main() {
    // Luo tm-struct (rakenne) nykyiselle ajalle
    time_t current_time;
    time(&current_time);

    // Tulostaa nykyisen ajan
    printf("Nykyinen aika: %s", ctime(&current_time));

    return 0;
}
```

Output käskyn ajamisen jälkeen:

```
Nykyinen aika: Mon Mar 29 19:12:24 2021
```

## Syvällisempi sukellus
`time.h` kirjasto sisältää useita hyödyllisiä funktioita ja rakenteita, jotka muokkaavat ja hallitsevat aikaa ja päivämääriä. Yksi näistä on `gmtime()` funktio, joka muuntaa ajan UTC-ajaksi (koordinoitu yleisaika) ja palauttaa pointerin `tm` rakenteeseen.

```C
#include <stdio.h>
#include <time.h>

int main() {
    // Luo tm-struct nykyiselle ajalle
    time_t current_time;
    time(&current_time);

    // Muuntaa ajan UTC-ajaksi ja tallentaa pointerin tm-structiin
    struct tm *gmtm = gmtime(&current_time);

    // Tulostaa UTC-ajan
    printf("UTC-aika: %s", asctime(gmtm));

    return 0;
}
```

Output käskyn ajamisen jälkeen:

```
UTC-aika: Tue Mar 30 02:12:24 2021
```

`time.h` kirjaston avulla voit myös manipuloida aikaa ja päivämääriä haluamallasi tavalla, kuten lisätä tai vähentää päiviä nykyisestä päivämäärästä. Tutustu tarkemmin kirjaston dokumentaatiosta löytyviin `time.h` funktioihin ja rakenteisiin saadaksesi lisätietoa.

## Katso myös
- [`time.h` kirjaston dokumentaatio (englanniksi)](https://www.gnu.org/software/libc/manual/html_node/Calendar-Time.html#Calendar-Time)
- [C-kieletutoriaalit (suomeksi)](https://www.cs.helsinki.fi/group/java/antti/c-opas-opettajille_index.html)