---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:11:53.273387-07:00
description: "Miten: Luetaan CSV-tiedosto C-kielell\xE4 k\xE4ytt\xE4m\xE4ll\xE4 standardin\
  \ tiedosto I/O -funktioita yhdess\xE4 merkkijonojen k\xE4sittelyfunktioiden kanssa\
  \ jokaisen rivin\u2026"
lastmod: '2024-03-13T22:44:57.065178-06:00'
model: gpt-4-0125-preview
summary: "Luetaan CSV-tiedosto C-kielell\xE4 k\xE4ytt\xE4m\xE4ll\xE4 standardin tiedosto\
  \ I/O -funktioita yhdess\xE4 merkkijonojen k\xE4sittelyfunktioiden kanssa jokaisen\
  \ rivin j\xE4sent\xE4miseksi."
title: "Ty\xF6skentely CSV:n kanssa"
weight: 37
---

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
