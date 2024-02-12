---
title:                "Työskentely CSV:n kanssa"
aliases:
- /fi/c/working-with-csv.md
date:                  2024-02-03T18:11:53.273387-07:00
model:                 gpt-4-0125-preview
simple_title:         "Työskentely CSV:n kanssa"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/working-with-csv.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Mikä & Miksi?

Ohjelmoinnissa CSV-tiedostojen (pilkuin erotetut arvot) käsittely tarkoittaa tietojen lukemista ja kirjoittamista tekstiedostoihin, jotka on järjestetty riveittäin, missä jokainen rivi edustaa tietuetta ja jokaisen tietueen kentät on erotettu pilkuilla. Ohjelmoijat manipuloivat CSV-tiedostoja helpottaakseen datan tuontia/vientiä eri järjestelmissä, niiden laajan tuen ja yksinkertaisuuden vuoksi taulukollisen datan tallennuksessa.

## Miten:

### CSV-tiedostojen lukeminen
Luetaan CSV-tiedosto C-kielellä käyttämällä standardin tiedosto I/O -funktioita yhdessä merkkijonojen käsittelyfunktioiden kanssa jokaisen rivin jäsentämiseksi. Alla on perusesimerkki CSV-tiedoston lukemisesta ja jokaisen rivin kenttien tulostamisesta konsoliin.

```c
#include <stdio.h>
#include <string.h>

int main() {
    FILE *fp = fopen("data.csv", "r");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char buf[1024];
    while (fgets(buf, 1024, fp)) {
        char *field = strtok(buf, ",");
        while(field) {
            printf("%s\n", field);
            field = strtok(NULL, ",");
        }
    }

    fclose(fp);
    return 0;
}
```
Esimerkki `data.csv`:
```
Name,Age,Occupation
John Doe,29,Software Engineer
```

Esimerkkitulo:
```
Name
Age
Occupation
John Doe
29
Software Engineer
```

### Kirjoittaminen CSV-tiedostoihin
Vastaavasti CSV-tiedostoon kirjoittaminen sisältää `fprintf` käytön datan tallentamiseksi pilkuin erotetussa muodossa.

```c
#include <stdio.h>

int main() {
    FILE *fp = fopen("output.csv", "w");
    if (!fp) {
        printf("Can't open file\n");
        return 1;
    }

    char *headers[] = {"Name", "Age", "Occupation", NULL};
    for (int i = 0; headers[i] != NULL; i++) {
        fprintf(fp, "%s%s", headers[i], (headers[i+1] != NULL) ? "," : "\n");
    }
    fprintf(fp, "%s,%d,%s\n", "Jane Doe", 27, "Data Scientist");

    fclose(fp);
    return 0;
}
```

Esimerkki `output.csv` Sisältö:
```
Name,Age,Occupation
Jane Doe,27,Data Scientist
```

## Syväsukellus

CSV-muoto, vaikkakin näennäisen suoraviivainen, sisältää hienovaraisuuksia, kuten kentissä esiintyvien pilkkujen käsittely ja kenttien sulkeminen lainausmerkeillä. Esitetyt perusesimerkit eivät huomioi tällaisia monimutkaisuuksia, eivätkä käsittele potentiaalisia virheitä robustisti.

Historiallisesti CSV-käsittely C-kielessä on suurelta osin ollut manuaalista johtuen kielen matalan tason luonteesta ja korkean tason abstraktioiden puutteesta tällaisiin tehtäviin. Tämä manuaalinen hallinta sisältää tiedostojen avaamisen, rivien lukemisen, merkkijonojen jakamisen ja tarpeen mukaan datatyyppien muuntamisen.

Vaikka suoranainen CSV-tiedostojen käsittely C-kielessä tarjoaa arvokkaita oppimiskokemuksia tiedosto I/O:sta ja merkkijonojen käsittelystä, useat modernit vaihtoehdot lupaavat tehokkuutta ja vähemmän virhealttiita prosesseja. Kirjastot kuten `libcsv` ja `csv-parser` tarjoavat kattavia funktioita CSV-tiedostojen lukemiseen ja kirjoittamiseen, mukaan lukien tuki lainausmerkeillä erotetuille kentille ja mukautetuille erottimille.

Vaihtoehtoisesti, kun työskennellään ekosysteemeissä, jotka sitä tukevat, yhdistäminen kielten tai alustojen kanssa, jotka tarjoavat korkean tason CSV-käsittelytoimintoja (kuten Python sen `pandas`-kirjaston kanssa), voi olla tuottavampi reitti sovelluksille, jotka vaativat runsasta CSV-käsittelyä. Tämä ristiinkielen lähestymistapa hyödyntää C:n suorituskykyä ja järjestelmäohjelmoinnin ominaisuuksia samalla kun käyttää hyödyksi muiden kielten käyttöhelppoutta erityistehtävissä, kuten CSV-käsittelyssä.
