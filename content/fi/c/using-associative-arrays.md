---
title:                "Assosiatiivisten taulukoiden käyttö"
date:                  2024-01-30T19:10:33.602512-07:00
model:                 gpt-4-0125-preview
simple_title:         "Assosiatiivisten taulukoiden käyttö"

category:             "C"
tag:                  "Data Structures"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-associative-arrays.md"
changelog:
  - 2024-01-30, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot tai hajautustaulut ovat avain-arvo-pareja, joiden avulla voit tallentaa ja hakea tietoja avaimen perusteella. Ne ovat erittäin hyödyllisiä C:ssä, koska ne mahdollistavat nopeamman tietojen pääsyn verrattuna listoihin, erityisesti kun käsitellään suurta määrää tietoja.

## Kuinka:

C ei tarjoa sisäänrakennettua tukea assosiatiivisille taulukoille toisin kuin jotkut muut kielet, mutta voimme käyttää rakenteita ja joitakin kirjastofunktioita saadaksemme samanlaisen toiminnallisuuden. Tässä on yksinkertainen toteutus käyttäen `uthash`-kirjastoa, jonka sinun tarvitsee sisällyttää projektiisi.

Ensimmäisenä, määrittele rakenne avain-arvo-parejasi varten:

```C
#include <stdio.h>
#include "uthash.h"

typedef struct {
    int id; // Tämä tulee olemaan meidän avaimemme
    char name[10]; // Tämä on arvo, joka liitetään avaimemme
    UT_hash_handle hh; // Mahdollistaa tämän rakenteen hajauttamisen
} henkilo;
```

Seuraavaksi, lisätään joitakin merkintöjä ja haetaan ne:

```C
int main() {
    henkilo *oma_henkilot = NULL, *s;

    // Lisätään merkintä
    s = (henkilo*)malloc(sizeof(henkilo));
    s->id = 1;
    strcpy(s->name, "Alice");
    HASH_ADD_INT(oma_henkilot, id, s);

    // Haetaan merkintä
    int kayttaja_id = 1;
    HASH_FIND_INT(oma_henkilot, &kayttaja_id, s);
    if (s) {
        printf("Löytyi: %s\n", s->name);
    }
    
    return 0;
}
```

Esimerkkituloste olisi:

```
Löytyi: Alice
```

Älä unohda vapauttaa varattua muistia ja vapauttaa hajautustaulua, kun olet valmis, jotta vältät muistivuodot.

## Syventävä tarkastelu

Vaikka assosiatiiviset taulukot eivät ole alkuperäisiä C:lle, kirjastot kuten `uthash` täyttävät aukon melko hyvin ja tarjoavat melko suoraviivaisen tavan käyttää tätä toiminnallisuutta. Historiallisesti C-kehittäjien on täytynyt toteuttaa omat versiot näistä tietorakenteista, mikä on johtanut vaihteleviin ja usein monimutkaisiin toteutuksiin, erityisesti niille, jotka vasta aloittavat kielen kanssa. 

Muista, että assosiatiivisten taulukoiden tehokkuus C:ssä riippuu suuresti siitä, kuinka hyvin hajautusfunktio jakaa arvot taulukkoon törmäysten minimoimiseksi. Vaikka kirjastot, kuten `uthash`, tarjoavat hyvän tasapainon helppokäyttöisyyden ja suorituskyvyn välillä, kriittisissä sovelluksissa, joissa suorituskyky on ensisijaisen tärkeää, saatat haluta räätälöidä tai toteuttaa oman hajautustaulusi.

Sovelluksiin, jotka vaativat maksimaalista tehokkuutta, vaihtoehtoiset tietorakenteet tai jopa muut ohjelmointikielet sisäänrakennetulla tuella assosiatiivisille taulukoille, saattavat olla parempi vaihtoehto. Kuitenkin monissa tilanteissa, erityisesti kun työskentelet jo C-ympäristössä, kirjaston kuten `uthash` käyttäminen tarjoaa käytännöllisen tasapainon suorituskyvyn ja mukavuuden välillä.
