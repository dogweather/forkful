---
title:                "CSV-tiedostojen käsittely"
date:                  2024-01-19
html_title:           "Bash: CSV-tiedostojen käsittely"
simple_title:         "CSV-tiedostojen käsittely"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
CSV on tekstitiedostomuoto datan tallentamiseen. Ohjelmoijat käyttävät sitä, koska se on yksinkertainen, yhteensopiva monien alustojen kanssa ja helppo muuttaa taulukkomuotoiseksi tiedoksi.

## How to: (Kuinka tehdään:)
```C
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// CSV-tiedoston lukeminen ja tulostaminen
int main() {
    FILE *fp = fopen("esimerkki.csv", "r");
    if (!fp) {
        printf("Tiedostoa ei voida avata.\n");
        return 1;
    }

    // Oleta, että yhdellä rivillä on korkeintaan 1024 merkkiä
    char rivi[1024];

    while (fgets(rivi, 1024, fp)) {
        // Leikkaa riviltä rivinvaihto
        rivi[strcspn(rivi, "\n")] = 0;
        // Tulosta rivi
        printf("%s\n", rivi);
    }

    fclose(fp);
    return 0;
}
```
Tuloste:
```
sarake1,sarake2,sarake3
data1,data2,data3
...
```

## Deep Dive (Syväsukellus):
CSV-formaatti juontaa juurensa varhaisiin tietokoneaikoihin, 1970-luvulle. Formaatti on pitänyt pintansa, vaikka nykyään on muitakin vaihtoehtoja kuten JSON ja XML. CSV:n käytön etuna on sen yksinkertaisuus; tiedostot ovat luettavia myös ihmisten silmin ja ne aukeavat helposti esimerkiksi Excelissä. Implementaatioissa haastetta tuo kenttärajaajien, kuten lainausmerkkien ja pilkkujen, käsittely.

## See Also (Katso myös):
- [RFC 4180, CSV standardi](https://tools.ietf.org/html/rfc4180)
- [C Standard Library](https://en.cppreference.com/w/c/header)
- [Stack Overflow CSV aiheiset kysymykset](https://stackoverflow.com/questions/tagged/csv?sort=frequent)
