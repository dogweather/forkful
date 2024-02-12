---
title:                "Nykyisen päivämäärän hankkiminen"
aliases: - /fi/c/getting-the-current-date.md
date:                  2024-02-03T17:57:38.810480-07:00
model:                 gpt-4-0125-preview
simple_title:         "Nykyisen päivämäärän hankkiminen"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Nykyisen päivämäärän saaminen C-kielessä tarkoittaa standardin C-kirjaston hyödyntämistä järjestelmän nykyisen ajan ja päivämäärän noutamiseen ja muotoilemiseen. Ohjelmoijat tarvitsevat tätä toiminnallisuutta usein lokitiedostojen, aikaleimojen tai suunniteltujen toimintojen toteuttamiseen sovelluksissaan.

## Kuinka:

C-kielessä otsikkotiedosto `<time.h>` tarjoaa tarvittavat funktiot ja tyypit päivämäärien ja aikojen käsittelyyn. `time()`-funktio hakee nykyisen ajan, kun taas `localtime()` muuntaa tämän ajan paikalliseen aikavyöhykkeeseen. Päivämäärän näyttämiseksi käytämme `strftime()`-funktiota sen muotoilemiseen merkkijonoksi.

Tässä on perusesimerkki:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char buffer[80];
    time_t rawtime;
    struct tm *timeinfo;

    // Hae nykyinen aika
    time(&rawtime);
    // Muunna se paikalliseksi ajaksi
    timeinfo = localtime(&rawtime);
    
    // Muotoile päivämäärä ja tulosta se
    strftime(buffer, 80, "Tämän päivän päivämäärä on %Y-%m-%d", timeinfo);
    printf("%s\n", buffer);

    return 0;
}
```

Esimerkkitulostus voisi näyttää tältä:

```
Tämän päivän päivämäärä on 2023-04-12
```

## Syväsukellus

C-kielessä ajan käsittely `<time.h>`:n kautta juontaa juurensa kielen ja UNIX-järjestelmien alkuaikoihin. Se on rakennettu `time_t`-tietotyypin ympärille, joka edustaa nykyistä aikaa sekuntien määränä Unix Epochista lähtien (1. tammikuuta 1970). Vaikka tämä on tehokasta ja universaalisti yhteensopivaa, se myös tarkoittaa, että standardin C-kirjaston aikafunktiot ovat perustavanlaatuisesti rajoittuneita `time_t`:n tarkkuuden ja vaihteluvälin suhteen.

Nykysovellukset, erityisesti ne, jotka vaativat korkean tarkkuuden aikaleimoja tai käsittelevät kaukaisia tai menneitä päivämääriä, saattavat kokea nämä rajoitukset haastaviksi. Esimerkiksi Vuosi 2038 -ongelma on kuuluisa esimerkki, jossa 32-bittistä `time_t`:a käyttävät järjestelmät ylivuotavat.

Monimutkaisemman ajan ja päivämäärien käsittelyn tarpeisiin monet ohjelmoijat kääntyvät ulkoisten kirjastojen tai käyttöjärjestelmän tarjoamien toimintojen puoleen. Esimerkiksi C++:ssa `<chrono>`-kirjasto tarjoaa tarkempia ja monipuolisempia ajan käsittelyn mahdollisuuksia.

Huolimatta rajoituksistaan, C-kielen aikafunktioiden yksinkertaisuus ja yleisyys tekevät niistä täysin sopivia moniin sovelluksiin. Näiden työkalujen ymmärtäminen on olennaista C-ohjelmoijille, tarjoten sekoituksen historiallista ohjelmointikontekstia ja käytännöllistä, jokapäiväistä hyötyä.
