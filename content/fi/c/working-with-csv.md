---
title:                "Työskentely csv:n kanssa"
html_title:           "C: Työskentely csv:n kanssa"
simple_title:         "Työskentely csv:n kanssa"
programming_language: "C"
category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-csv.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

CSV on yksinkertainen tiedostomuoto, jota käytetään datan tallentamiseen taulukkoihin. Se koostuu arvoista, jotka on erotettu pilkulla ja riveistä, jotka on erotettu rivinvaihdolla. Ohjelmoijat käyttävät CSV:tä, koska se on helppo luoda ja lukea, ja sitä voidaan käyttää erilaisiin tietojen tallennustarkoituksiin.

## Kuinka:

```C
#include <stdio.h>

int main()
{
  FILE *csv_file;
  int value;
  
  csv_file = fopen("data.csv", "r");
  
  if (csv_file == NULL)
  {
    printf("Tiedostoa ei voitu avata!");
    return 1;
  }
  
  // Tulostetaan tiedoston sisältö
  while (fscanf(csv_file, "%d,", &value) != EOF)
  {
    printf("%d ", value);
  }
  
  fclose(csv_file);
  
  return 0;
}
```

**Esimerkki sisäänmenosta:**

```
1,2,3
4,5,6
```

**Esimerkki tulosteesta:**

```
1 2 3 4 5 6
```

## Syvemmälle:

CSV kehitettiin vuonna 1972, joten se on ollut käytössä yli 40 vuotta. Tänä päivänä on myös muita vaihtoehtoja, kuten JSON ja XML, jotka tarjoavat enemmän ominaisuuksia, mutta ovat monimutkaisempia käyttää. CSV on edelleen suosittu vaihtoehto yksinkertaisiin taulukkolaskentatarkoituksiin.

CSV-tiedoston sisällä voi olla myös erilaisia erottimia, kuten puolipiste tai tabulaattori, ja se voi aiheuttaa ongelmia tiedoston lukemisessa. On tärkeää käsitellä näitä erikoistapauksia ja varmistaa, että CSV-tiedoston käsittely on joustavaa.