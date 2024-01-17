---
title:                "Tekstitiedoston kirjoittaminen"
html_title:           "C: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?
Kirjoittaminen teksti-tiedostoon on tiedon tallentamista tietokoneelle tekstimuodossa. Koodaajat käyttävät teksti-tiedostoja tallentaakseen koodin ja sen tulokset.

## Miten:
```
#include <stdio.h>
int main() {
    char text[] = "Tämä on esimerkki teksti-tiedostoon kirjoituksesta.";
    // Luodaan osoitin tiedostoon ja avataan se kirjoittamista varten
    FILE *fptr;
    fptr = fopen("tekstiedosto.txt", "w");

    // Kirjoitetaan teksti teksti-tiedostoon
    fprintf(fptr, "%s", text);

    // Suljetaan tiedosto
    fclose(fptr);

    // Tulostetaan onnistumisviesti
    printf("Teksti on kirjoitettu tiedostoon.");

    return 0;
}
```

```
Tulostus tiedostoon "tekstiedosto.txt":
Tämä on esimerkki teksti-tiedostoon kirjoituksesta.
```

## Syvällinen sukellus:
Kirjoittaminen teksti-tiedostoon on ollut yksi olennaisimmista tiedon tallentamisen tavoista ohjelmoinnin alkuaikoina. Nykyään on olemassa monia muita tapoja tallentaa tietoa, kuten tietokantoihin tai verkkopalveluihin. Kirjoittaminen teksti-tiedostoon on kuitenkin edelleen tärkeää monissa tilanteissa, esimerkiksi yksinkertaisissa ohjelmissa tai tulosten tallentamisessa koodauksen aikana. Kirjoittamalla teksti-tiedostoon voit myös helposti muokata tiedostoa manuaalisesti.

## Katso myös:
- [Tiedoston avaaminen kirjoittamista varten C-kielessä](https://www.programiz.com/c-programming/c-file-input-output)
- [Fopen-funktion dokumentaatio](https://www.cplusplus.com/reference/cstdio/fopen/)