---
title:                "Satunnaislukujen generointi"
date:                  2024-01-27T20:32:51.509083-07:00
model:                 gpt-4-0125-preview
simple_title:         "Satunnaislukujen generointi"
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?

Satunnaislukujen tuottaminen C:ssä tarkoittaa lukujen jaksojen luomista, joista ei löydy mitään havaittavissa olevaa kaavaa, jäljitellen satunnaisuuden käsitettä. Ohjelmoijat hyödyntävät satunnaislukuja monenlaisiin tarkoituksiin, mukaan lukien datan simuloiminen, kryptografiset sovellukset ja pelikehitys, mikä tekee siitä olennaisen osan ohjelmointia.

## Miten:

Satunnaislukujen tuottamiseksi C:ssä käytetään yleensä `stdlib.h`-kirjastosta löytyvää `rand()`-funktiota. On kuitenkin tärkeää alustaa satunnaislukugeneraattori varmistaakseen muuttuvuuden tuotetuissa luvuissa eri ohjelman suorituskertojen välillä. `Srand()`-funktio, alustettuna arvolla, usein nykyisellä ajalla, mahdollistaa tämän.

Tässä yksinkertainen esimerkki satunnaisluvun tuottamisesta välillä 0 ja 99:

```c
#include <stdio.h>
#include <stdlib.h>
#include <time.h>

int main() {
    // Alusta satunnaislukugeneraattori
    srand((unsigned) time(NULL));

    // Tuota satunnaisluku välillä 0 ja 99
    int randomNumber = rand() % 100;

    // Tulosta satunnaisluku
    printf("Random Number: %d\n", randomNumber);

    return 0;
}
```

Esimerkkituloste:

```
Random Number: 42
```

On tärkeää huomata, että jokainen tämän ohjelman suoritus tuottaa uuden satunnaisluvun, kiitos nykyisellä ajalla tehdyn alustuksen.

## Syväsukellus

Perinteinen tapa tuottaa satunnaislukuja C:ssä käyttäen `rand()`- ja `srand()`-funktioita, ei ole todella satunnainen. Se on pseudosatunnainen. Tämä riittää monille sovelluksille, mutta se ei ole riittävä tilanteissa, jotka vaativat suurta satunnaisuuden astetta, kuten vakavissa kryptografisissa käytöissä. `Rand()`-funktiolla tuotettu sekvenssi määräytyy kokonaan `srand()`-funktiolla annetun siemenen perusteella. Näin ollen, jos siemen on tiedossa, sekvenssi voidaan ennustaa, mikä vähentää satunnaisuutta.

Historiallisesti `rand()`-funktiota on kritisoitu sen heikkolaatuisesta satunnaisuudesta ja rajallisesta alueesta. Modernit vaihtoehdot sisältävät laitespesifisten API:en käyttämisen tai ulkoisten kirjastojen, jotka paremmin lähestyvät todellista satunnaisuutta, tai UNIX-kaltaisissa järjestelmissä, lukemisen `/dev/random` tai `/dev/urandom` tiedostoista kryptografisia tarkoituksia varten.

Esimerkiksi käyttäen `/dev/urandom` C:ssä:

```c
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *fp;
    unsigned int randomNumber;

    // Avaa /dev/urandom lukemista varten
    fp = fopen("/dev/urandom", "r");

    // Lue satunnaisluku
    fread(&randomNumber, sizeof(randomNumber), 1, fp);

    // Tulosta satunnaisluku
    printf("Random Number: %u\n", randomNumber);

    // Sulje tiedosto
    fclose(fp);

    return 0;
}
```

Tämä menetelmä lukee suoraan järjestelmän entropia-altaasta, tarjoten korkealaatuisempaa satunnaisuutta herkempiin sovelluksiin. Tällä lähestymistavalla saattaa kuitenkin olla siirrettävyysongelmia eri alustoilla, mikä tekee siitä vähemmän universaalin kuin `rand()`-funktion käytön.

Riippumatta menetelmästä, satunnaisuuden luonteen ja sen toteuttamisen C:ssä ymmärtäminen on ratkaisevan tärkeää tehokkaiden, turvallisten ja mukaansatempaavien sovellusten kehittämisessä.
