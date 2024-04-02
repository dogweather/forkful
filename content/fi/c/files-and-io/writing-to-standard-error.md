---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:04.524762-07:00
description: "Virheiden kirjoittaminen standardivirheeseen C:ss\xE4 tarkoittaa virheilmoitusten\
  \ ja diagnostiikkatiedon ohjaamista erilliseen virtaan p\xE4\xE4ohjelman\u2026"
lastmod: '2024-03-13T22:44:57.058286-06:00'
model: gpt-4-0125-preview
summary: "Virheiden kirjoittaminen standardivirheeseen C:ss\xE4 tarkoittaa virheilmoitusten\
  \ ja diagnostiikkatiedon ohjaamista erilliseen virtaan p\xE4\xE4ohjelman\u2026"
title: Kirjoittaminen standardivirheeseen
weight: 25
---

## Mikä & Miksi?

Virheiden kirjoittaminen standardivirheeseen C:ssä tarkoittaa virheilmoitusten ja diagnostiikkatiedon ohjaamista erilliseen virtaan pääohjelman tulosteesta. Ohjelmoijat tekevät näin erottaakseen virheilmoitukset tavallisesta tulosteesta, mikä tekee molemmista helpommin luettavia ja erikseen käsiteltäviä, erityisesti ohjelmien suorituksen vianmäärityksessä tai lokitiedostojen kirjaamisessa.

## Kuinka tehdä:

C:ssä `stderr`-virtaa käytetään virheilmoitusten kirjoittamiseen. Toisin kuin standarditulosteeseen kirjoittaminen `printf`-funktiolla, virheviestien kirjoittaminen `stderr`-virtaan voidaan tehdä käyttäen `fprintf`- tai `fputs`-funktioita. Tässä on miten voit tehdä sen:

```c
#include <stdio.h>

int main() {
    fprintf(stderr, "Tämä on virheilmoitus.\n");

    fputs("Tämä on toinen virheilmoitus.\n", stderr);
    
    return 0;
}
```

Näytekirjoitus (stderriin):
```
Tämä on virheilmoitus.
Tämä on toinen virheilmoitus.
```

On tärkeää huomata, että vaikka tuloste näyttää samanlaiselta kuin `stdout` konsolissa, kun ohjelmaa käytetään terminaalissa uudelleenohjauksen avulla, ero tulee selkeäksi:

```sh
$ ./your_program > output.txt
```

Tämä komento ohjaa vain tavallisen tulosteen `output.txt`-tiedostoon, kun taas virheilmoitukset näkyvät edelleen ruudulla.

## Syväsukellus

Ero `stdout`- ja `stderr`-virtausten välillä Unix-pohjaisissa järjestelmissä juontaa juurensa C:n ja Unixin alkuaikoihin. Tämä erottelu mahdollistaa vankemman virheenkäsittelyn ja lokitusten, koska se antaa ohjelmoijille mahdollisuuden ohjata virheilmoitukset erillään tavallisesta ohjelman tulosteesta. Vaikka `stderr` on oletusarvoisesti puskuroimaton varmistaen virheilmoitusten välittömän tulostuksen, mikä auttaa vianmäärityksessä kaatuessa ja muissa kriittisissä ongelmissa, `stdout` on tyypillisesti puskuroitu, mikä tarkoittaa, että sen tulostus saattaa viivästyä kunnes puskuri huuhdellaan (esim. ohjelman suorituksen päätyttyä tai manuaalisesti).

Nykysovelluksissa `stderr`-virtaan kirjoittaminen on edelleen relevanttia, erityisesti komentorivityökaluissa ja palvelinsovelluksissa, joissa tavallisten lokiviestien ja virheiden erottaminen on olennaista. Kuitenkin monimutkaisemmissa virheenkäsittelytilanteissa, erityisesti GUI-sovelluksissa tai kun tarvitaan hienostuneempia lokitusmekanismeja, ohjelmoijat saattavat käyttää dedikoituja lokituskirjastoja, jotka tarjoavat enemmän kontrollia viestien muotoiluun, kohdistamiseen (esim. tiedostot, verkko) ja tärkeysasteisiin (info, varoitus, virhe jne.).

Vaikka `stderr` tarjoaa perusmekanismin virheraportointiin C:ssä, ohjelmointikäytäntöjen kehittyminen ja edistyneiden lokituskehysten saatavuus tarkoittavat usein, että se on vain lähtökohta nykyaikaisille virheenkäsittelystrategioille.
