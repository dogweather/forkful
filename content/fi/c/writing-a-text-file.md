---
title:    "C: Tiedoston kirjoittaminen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstitiedostojen kirjoittaminen on tärkeä osa ohjelmistokehitystä ja data-analyysiä. Se mahdollistaa tietojen tallentamisen ja jakamisen eri muodoissa, ja on siksi välttämätöntä monissa ohjelmoinnin projekteissa.

## Kuinka

Seuraavassa esimerkissä näytetään, kuinka voit kirjoittaa tekstitiedoston C-kielellä käyttämällä `fopen`-funktiota ja `fprintf`-komenneta:

```C
#include <stdio.h>

int main()
{
    FILE *tiedosto = fopen("teksti.txt", "w"); //Avaa tiedoston kirjoitusta varten
    if (tiedosto != NULL)
    {
        //Kirjoita tekstitiedostoon käyttäen fprintf-komentoa
        fprintf(tiedosto, "Tämä on tekstiä, joka tallentuu tiedostoon.");
        fclose(tiedosto); //Sulje tiedosto
        printf("Tekstitiedosto on kirjoitettu onnistuneesti.");
    }
    else
    {
        printf("Tiedoston avaaminen epäonnistui.");
    }

    return 0;
}
```

Esimerkin avulla olemme luoneet uuden tekstitiedoston nimeltä "teksti.txt". Käytämme `fprintf`-komenneta kirjoittaaksemme tiedostoon ja `fclose`-komenneta sulkeaksemme tiedoston. Lopuksi tulostetaan viesti, joka kertoo onnistuneesta tiedoston kirjoittamisesta.

## Syvemmälle

Tekstitiedostojen kirjoittamisessa on tärkeää ymmärtää muutamia käsitteitä. Ensinnäkin, jokaiselle tiedostolle on annettava "tiedoston osoitin" eli muuttuja, jota käytetään ohjelmassa tiedoston käsittelyyn. Tässä esimerkissä käytimme `FILE *tiedosto` muuttujaa.

Toiseksi, on tärkeää tietää mitkä argumentit tarvitaan `fopen`-funktiolle. Ensimmäinen argumentti on tietysti tiedoston nimi ja toinen on avausmuoto. "w" avausmuoto tarkoittaa, että tiedosto avataan kirjoitusta varten.

Lisäksi `fprintf`-komennolla on monia käyttömahdollisuuksia, esimerkiksi voit tulostaa useita muuttujia samassa tiedostossa. Löydät lisätietoja C-kielestä ja sen toiminnoista dokumentaatiosta.

## Katso myös

- Dokumentaatio `fopen`-funktiosta: https://www.tutorialspoint.com/c_standard_library/c_function_fopen.htm
- Dokumentaatio `fprintf`-komennosta: https://www.tutorialspoint.com/c_standard_library/c_function_fprintf.htm
- C-kielen dokumentaatio: https://www.tutorialspoint.com/cprogramming/index.htm