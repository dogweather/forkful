---
title:                "Tekstitiedoston lukeminen"
html_title:           "Lua: Tekstitiedoston lukeminen"
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Luettaessa Tekstitiedostoja C-ohjelmoinnissa

## Mikä & Miksi?
Tekstitiedoston lukeminen on prosessi, jossa ohjelma lukee merkkejä tiedostosta ja käsittelee niitä. Tämä on hyödyllistä, kun moitteettomasti varastoituja tietoja halutaan hyödyntää ohjelman toiminnassa.

## Kuinka Toimia:
Alla on esimerkki siitä, kuinka lukea tekstitiedostoa C-ohjelmointikielellä.

```C
#include <stdio.h>

int main(){
    FILE *file;
    char c;

    file = fopen("tiedosto.txt", "r");
    if(file == NULL){
        printf("Tiedostoa ei voida avata!\n");
        return 1;
    }

    while((c = fgetc(file)) != EOF){
        printf("%c", c);
    }

    fclose(file);
    return 0;
}
```

Tämä ohjelma avaa "tiedosto.txt", lukee sen sisällön merkki kerrallaan ja tulostaa sen. Jos tiedostoa ei voida avata, se tulostaa virheviestin ja lopettaa.

## Syvempi Sukellus
Historiallisesti C on tarjonnut tiedostonkäsittelytoiminnot, mukaan lukien luku, kirjoitus, luominen ja poistaminen. `FILE *` pointteri ja `fopen`, `fgetc`, ja `fclose` funktiot ovat osa tämän ominaisuuden tarjoamista välineitä.

Vaihtoehtoisesti, voit käyttää `fscanf` tai `fgets` funktioita lukemaan kokonaisen rivin kerrallaan tai formatoidun syötteen. Valinta riippuu tiedostosi sisällöstä ja siitä, kuinka haluat käsitellä sen.

Tiedoston käsittelemiseen liittyy resurssienhallinta, mm. tiedoston avaaminen ja sulkeminen. Jos et sulje tiedostoa, saatat aiheuttaa resurssivuotoja, jotka kuluttavat muistia tarpeettomasti.

## Katso Myös
Lisätietoja ja lähteitä:
- [C Library - <stdio.h>](https://www.tutorialspoint.com/c_standard_library/stdio_h.htm)
- [File I/O in C programming with examples](https://beginnersbook.com/2014/01/c-file-io/)
- [C Programming Files I/O](https://www.learn-c.org/en/Files_IO)