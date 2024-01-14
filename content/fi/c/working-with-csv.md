---
title:                "C: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-csv.md"
---

{{< edit_this_page >}}

# Miksi käyttäisit CSV-tiedostoja C-ohjelmoimiseen?

CSV-tiedostot ovat yksi yleisimmistä tiedostomuodoista tietojen tallentamisessa ja jakamisessa. Niitä käytetään usein yritysten ja organisaatioiden välisessä tiedonvaihdossa sekä tietokantojen tallentamisessa. CSV-tiedostojen käsittelyyn liittyy tärkeitä taitoja, jotka ovat arvokkaita C-ohjelmoijille.

## Kuinka käsitellä CSV-tiedostoja C-ohjelmoinnissa?

Tiedostojen käsittely C-ohjelmoinnissa vaatii joitain tärkeitä ymmärryksiä, kuten tiedostojen avaamisen, lukemisen ja kirjoittamisen. CSV-tiedostojen käsittelyssä on myös muita erityisiä tekniikoita, joita tarvitaan tietojen tehokkaaseen lukemiseen ja tallentamiseen.

Tässä on yksinkertainen esimerkki, joka lukee ja tulostaa CSV-tiedostosta tietoja käyttäen C:n file-luokkaa ja fscanf-funktiota:

```
#include <stdio.h>

int main() {
   FILE *fptr;
   fptr = fopen("test.csv", "r");
   
   if(fptr == NULL) {
      printf("Tiedoston avaaminen epäonnistui.");
      return 1;
   }
   
   int id;
   char name[20];
   float salary;
   fscanf(fptr, "%d,%s,%f", &id, name, &salary);
   
   printf("ID: %d \n", id );
   printf("Name: %s \n", name );
   printf("Salary: %.2f", salary);

   fclose(fptr); 
  
   return 0;
}
```

Esimerkin CSV-tiedoston sisältö:

```
1, John, 2000.00
2, Jane, 2500.50
```

Koodin tulostama tulos:

```
ID: 1
Name: John
Salary: 2000.00
```

## Syvällisempi sukellus CSV-tiedostojen käsittelyyn

CSV-tiedostojen käsittely voi olla monimutkaista riippuen tiedostojen sisällöstä. C-ohjelmoinnilla on kyky käsitellä muotoillun merkkijonon lukemista ja kirjoittamista. Tämä tekee CSV-tiedostojen lukemisesta ja kirjoittamisesta melko helppoa.

Kuitenkin, jos CSV-tiedoston tiedot sisältävät esimerkiksi pilkkuja tai muita erikoismerkkeja, joudut käsittelemään niitä eri tavalla, jotta tiedot luetaan oikein. Tämä vaatii lisätyötä, mutta C-ohjelmoinnilla on monia kirjastoja ja toimintoja, jotka voivat auttaa tässä prosessissa.

Lisäksi voit käyttää korkeamman tason ohjelmointikieliä, kuten Pythonia tai Javasciptiä, jotta CSV-tiedostojen käsittely olisi helpompaa ja vähemmän monimutkaista.

# Katso myös

- [CSV-tiedostojen lukeminen ja kirjoittaminen C-ohjelmoinnissa](https://www.programiz.com/c-programming/c-file-input-output)
- [C: avaa-tiedostokomento](https://www.tutorialspoint.com/c_standard_library/c_function_fscanf.htm)
- [CsvReader-kirjasto](https://sourceforge.net/projects/cccsvparser/)