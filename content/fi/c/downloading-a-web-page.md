---
title:                "Verkkosivun lataaminen"
html_title:           "C: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "C"
category:             "C"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi
Viime vuosien aikana internetin käyttö on kasvanut huomattavasti ja se on tullut keskeiseksi osaksi monen ihmisen arkea. Tämän vuoksi on tärkeää pystyä lataamaan verkkosivuja ja käsittelemään niiden tietoja. Tässä artikkelissa opit, kuinka voit ladata verkkosivun C-ohjelmoinnilla ja mitä hyötyä siitä voi olla.

## Kuinka
Lataaminen on yksi yleisimmistä tehtävistä C-ohjelmoinnissa ja sen avulla voidaan hakea tietoa verkkosivuilta ja käsitellä sitä ohjelman sisällä. Ensimmäiseksi tarvitset ohjelmaan mukaan tarvittavat tiedostot, kuten <stdio.h> ja <stdlib.h>, sekä mukana oleva osoite, josta sivu halutaan ladata.

```
#include <stdio.h>
#include <stdlib.h>

int main() {
    // osoite, josta sivu ladataan
    char *osoite = "https://example.com";
    // tiedosto, johon sivun tiedot tallennetaan
    FILE *tiedosto = fopen("sivu.html", "w");
    // C-komento lataukseen
    system("curl -o sivu.html https://example.com");
    // tiedoston sulkeminen
    fclose(tiedosto);
    return 0;
}
```

Koodin ensimmäisillä riveillä määritetään tarvittavat tiedostot ja sitten määritellään muuttuja osoitteelle, josta sivu ladataan. Tämän jälkeen luodaan muuttuja tiedostolle, johon sivun tiedot tallennetaan. Koodin suorituksen aikana käytetään C-komentoa `system()` latauksen tekemiseen ja tallennetaan sivun tiedot annettuun tiedostoon. Lopuksi tiedosto suljetaan ja ohjelma palaa pääohjelmaan.

## Syventymistä
Lataamalla verkkosivun C-ohjelmalla, voidaan käsitellä sivun tietoja ja poimia sieltä haluttuja tietoja. Tämä voi olla hyödyllistä esimerkiksi verkkosivujen tarkkailemisessa tai tietojen keräämisessä. Lisäksi lataamalla sivun ohjelman kautta, voidaan automatisoida prosesseja ja säästää aikaa.

## Katso myös
- [C-kielen virallinen dokumentaatio](https://www.gnu.org/software/gnu-c-manual/gnu-c-manual.html)
- [C-ohjelmoinnin perusteet](https://www.studytonight.com/c/)