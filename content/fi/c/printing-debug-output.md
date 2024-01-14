---
title:                "C: Virheilmoitusten tulostaminen"
simple_title:         "Virheilmoitusten tulostaminen"
programming_language: "C"
category:             "C"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Debug-tulosteiden tulostaminen on erittäin tärkeä osa C-ohjelmointia, jota kaikki kehittäjät joutuvat tekemään. Tulosteiden tulostaminen voi auttaa tunnistamaan ja korjaamaan ohjelmassa olevia virheitä, mikä tekee siitä välttämättömän osan ohjelmien kehittämistä.

## Kuinka

Käytännössä debug-tulosteiden tulostaminen C-ohjelmassa tapahtuu käyttämällä printf-funktiota. Tämä funktio ottaa ensimmäisenä parametrina tulostusmuodon ja seuraavina parametreina tulostettavat arvot, näin:

```C
printf("Arvo 1: %d, Arvo 2: %s", int_arvo, string_arvo);
```

Tämä koodi vähentää ensin int_arvon, ja sitten string_arvon, ensimmäinen muunnetaan ja tulostetaan numeroksi prosenttimerkkiä seuraavan "d":n avulla, ja toinen merkkijonoksi "s":n avulla.

Tämä on hyvä tapa tulostaa debug-tulosteita, koska se antaa sinulle mahdollisuuden tulostaa kunkin arvon erikseen. Voit myös lisätä kontrollirakenteita, kuten if-lausekkeita ja while-silmukoita, jotta voit säätää tulosteiden tulostamista tarpeittesi mukaan.

## Syväsukellus

Printf-funktiota käyttävät monet muutkin C:n sisäänrakennetut funktiot, kuten strcpy ja scanf. Tämä johtuu siitä, että printf tarjoaa hyödyllisiä muotoilusymboleja, kuten "%d" ja "%s", jotta voit muuntaa muuttujat ja tulostaa ne oikeassa muodossa. Voit myös luoda omia muotoilusymboleja määrittelemällä makron, esimerkiksi:

```C
#define INT "["%d"]"

printf(INT, int_arvo);
```

Näin voit määrittää ja käyttää omia muotoilusymbolejasi sopimaan tarpeisiisi.

## Katso myös

- [C-kielen perusteet](https://www.w3schools.com/c/)
- [C-kielen käsikirja](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html)
- [C-kielen debuggaaminen](https://stackoverflow.com/questions/18480470/how-to-debug-a-c-program-using-gdb-instructions)