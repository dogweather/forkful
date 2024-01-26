---
title:                "Päivämäärän muuntaminen merkkijonoksi"
date:                  2024-01-20T17:36:03.917943-07:00
model:                 gpt-4-1106-preview
simple_title:         "Päivämäärän muuntaminen merkkijonoksi"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Muunnamme päivämääriä merkkijonoiksi esittääksemme ne ymmärrettävässä muodossa. Ohjelmoijat tekevät tämän, jotta päivämäärätiedot ovat käyttäjille selvempiä ja lokitietueet yhtenäisiä.

## How to: (Kuinka Tehdään:)
C:ssä voi käyttää `strftime`-funktiota päivämäärän muuntamiseen merkkijonoksi. Aloitetaan.

```C
#include <stdio.h>
#include <time.h>

int main() {
    time_t nyt = time(NULL);
    struct tm *aika = localtime(&nyt);
    
    char paivamaara[20];
    strftime(paivamaara, sizeof(paivamaara), "%d.%m.%Y", aika);
    
    printf("Tänään on: %s\n", paivamaara);
    return 0;
}
```

Mikäli tänään on 15. huhtikuuta 2023, tulostus on:
```
Tänään on: 15.04.2023
```

## Deep Dive (Sukellus Syvyyksiin):
`strftime` tulee historiallisesti UNIX-järjestelmistä. C standardikirjasto otti sen käyttöön, koska se osoittautui hyödylliseksi. Vaihtoehtoisesti voidaan käyttää `sprintf` funktiota, mutta se edellyttää, että muistamme päivämäärien formaatin. `strftime`-funktio tukee erilaisia aikamuotoja ja se on helpommin käytettävissä kansainvälisten formaattien kanssa. 

Implementaatiotiedot: `strftime` käyttää `tm`-rakennetta, joka sisältää päivämäärä- ja aikatietoja. `time_t` tyyppi taas on kalenteriaika, sekunteina epookista (1.1.1970) alkaen.

## See Also (Katso Myös):
- C Standard Library – `time.h`: https://en.cppreference.com/w/c/chrono
- `strftime` format specifierit: https://www.cplusplus.com/reference/ctime/strftime/
