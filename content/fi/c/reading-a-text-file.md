---
title:    "C: Tiedostotekstin lukeminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Yksi yleisiä tehtäviä C-ohjelmointikielessä on tiedostojen lukeminen ja niiden sisällön käsittely. On tärkeää tietää, miten tätä tehtävää suoritetaan, jotta voidaan parantaa ohjelman tehokkuutta ja luoda monimutkaisempia sovelluksia.

## Kuinka

Tiedostojen lukeminen C-kielellä tapahtuu käyttämällä `FILE`-rakennetta ja siihen liittyviä funktioita, kuten `fopen()`ja`fscanf()`. Alla on esimerkki yksinkertaisesta ohjelmasta, joka lukee tekstitiedoston ja tulostaa sen sisällön:

```C
#include <stdio.h>

int main() {
    // avataan tiedosto "teksti.txt" lukemista varten
    FILE *tiedosto = fopen("teksti.txt", "r");

    // tarkistetaan, että tiedosto avattiin onnistuneesti
    if (tiedosto == NULL) {
        printf("Tiedoston avaaminen epäonnistui");
        return 1; // keskeytetään ohjelma
    }

    // luetaan tiedostosta merkkejä ja tulostetaan ne näytölle
    char merkki;
    while((merkki = fgetc(tiedosto)) != EOF) {
        printf("%c", merkki);
    }

    // suljetaan tiedosto
    fclose(tiedosto);
    return 0;
}
```
Esimerkiksi, jos tiedoston "teksti.txt" sisältö on "Tämä on esimerkkiteksti", ohjelma tulostaa:

```
Tämä on esimerkkiteksti
```

## Syvempi sukellus

Tiedostojen lukeminen voidaan tehdä myös riviväleillä ja sanojen väleillä määrittelemällä sopivat muuttujat ja käyttämällä `fgets()`- ja `sscanf()`-funktioita. Tämä antaa enemmän hallintaa tiedoston käsittelyyn ja mahdollistaa esimerkiksi tiettyjen rivien tai sanojen poimimisen.

On myös tärkeää muistaa, että tiedostot tulee aina sulkea käytön jälkeen `fclose()`-funktiolla, jotta varmistetaan, että resurssit vapautuvat oikein.

## Katso myös

- [C käsky Rakenne](https://www.tutorialspoint.com/cprogramming/c_structures.htm)
- [C Tiedostojen käsittely](https://www.studytonight.com/c/file-handling-in-c.php)
- [C-kielen virallinen dokumenttointi](https://devdocs.io/c/)