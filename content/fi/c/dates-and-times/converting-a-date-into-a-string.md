---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:54:07.537895-07:00
description: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi C:ss\xE4 tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4rakenteen tai aikaleiman k\xE4\xE4nt\xE4mist\xE4 ihmisen\
  \ luettavaan muotoon. Ohjelmoijat suorittavat\u2026"
lastmod: '2024-03-13T22:44:57.052712-06:00'
model: gpt-4-0125-preview
summary: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuntaminen merkkijonoksi C:ss\xE4 tarkoittaa\
  \ p\xE4iv\xE4m\xE4\xE4r\xE4rakenteen tai aikaleiman k\xE4\xE4nt\xE4mist\xE4 ihmisen\
  \ luettavaan muotoon. Ohjelmoijat suorittavat\u2026"
title: "P\xE4iv\xE4m\xE4\xE4r\xE4n muuttaminen merkkijonoksi"
weight: 28
---

## Mitä ja Miksi?

Päivämäärän muuntaminen merkkijonoksi C:ssä tarkoittaa päivämäärärakenteen tai aikaleiman kääntämistä ihmisen luettavaan muotoon. Ohjelmoijat suorittavat usein tämän tehtävän näyttääkseen päivämääriä lokeissa, käyttöliittymissä tai tallentaessaan päivämääriä tekstipohjaiseen muotoon kuten JSON tai CSV.

## Kuinka:

`<time.h>` kirjaston `strftime` funktiota käytetään yleisesti tähän tarkoitukseen. Se mahdollistaa päivämäärän ja ajan muotoilun monin eri tavoin määrittämällä muotoiluspesifikaattoreita. Tässä on nopea esimerkki:

```c
#include <stdio.h>
#include <time.h>

int main() {
    char dateStr[100];
    time_t now = time(NULL);
    struct tm *ptm = localtime(&now);

    // Muunna päivämäärä & aika merkkijonoksi (esim. "Wed Jun 30 21:49:08 2021")
    strftime(dateStr, sizeof(dateStr), "%a %b %d %H:%M:%S %Y", ptm);
    
    printf("Nykyinen päivämäärä ja aika: %s\n", dateStr);
    return 0;
}
```

Esimerkkituloste voisi näyttää tältä:

```
Nykyinen päivämäärä ja aika: Wed Jun 30 21:49:08 2021
```

Voit räätälöidä muodon vaihtamalla `strftime` -funktiolle annettavia muotoiluspesifikaattoreita. Esimerkiksi saadaksesi päivämäärän muodossa `VVVV-KK-PP`, käyttäisit `"Y-%m-%d"`.

## Syväsukellus

`strftime` funktio ja `<time.h>` kirjasto ovat osa C Standard Kirjastoa, joka juontaa juurensa alkuperäiseen ANSI C standardiin (C89/C90). Vaikka suoraviivainen ja laajalti tuettu monilla alustoilla, tämä lähestymistapa voi tuntua matalan tason ja hankalalta verrattuna moderneihin ohjelmointikieliin, jotka tarjoavat intuitiivisempia päivämäärä- ja aikakirjastoja.

Tulisi huomata, vaikkakin C standardikirjaston aikatoiminnot ovat laajalti tuetut ja suhteellisen yksinkertaisia käyttää, ne kaipaavat joitakin monimutkaisempia aikavyöhykkeiden käsittelyn ja kansainvälistämisen ominaisuuksia, jotka löytyvät uudempien kielten kirjastoista tai kolmansien osapuolien C kirjastoista, kuten International Components for Unicode (ICU).

Kuitenkin, `strftime` funktion muokkausominaisuudet ja laaja alustatuki tekevät siitä luotettavan ja hyödyllisen työkalun päivämäärämerkkijonon muuntamiseen C:ssä. Ohjelmoijien, jotka tulevat kielistä, joissa on korkeamman tason datetime-kirjastoja, saattaa olla tarpeen sopeutua sen matalan tason luonteeseen, mutta he huomaavat sen erittäin tehokkaaksi ja monipuoliseksi välineeksi päivämäärien ja aikojen muotoiluun monenlaisiin sovelluksiin.
