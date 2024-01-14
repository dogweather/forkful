---
title:                "C: Tarkistetaan, onko kansio olemassa"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi voisi olla hyödyllistä tarkistaa, onko hakemisto olemassa. Yksi mahdollinen syy voisi olla, jos ohjelma tarvitsee tietyn hakemiston olemassaoloa suorittaakseen tiettyjä toimintoja. Toinen syy voisi olla, että halutaan varmistaa, ettei ohjelmalle aiheudu virheitä, jos hakemistoa ei ole olemassa.

## Miten

Hakemiston olemassaolon tarkistaminen voidaan toteuttaa C-ohjelmointikielellä käyttämällä `opendir`-funktiota ja `stat`-rakennetta. Alla on esimerkki koodista, joka tarkistaa, onko hakemisto "my_directory" olemassa ja tulostaa sen jälkeen vastaavan viestin.

```C
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <dirent.h>

int main() {
    char* dir_name = "my_directory";

    // Tarkistetaan, onko hakemisto olemassa
    DIR* dir = opendir(dir_name);
    if (dir) {
        // Hakemisto on olemassa
        printf("%s on olemassa.\n", dir_name);
        closedir(dir);
    } else {
        // Hakemistoa ei ole olemassa
        printf("%s ei ole olemassa.\n", dir_name);
    }

    return 0;
}
```
Esimerkkilähtö:
```
my_directory on olemassa.
```

## Syvemmälle

Hakemiston olemassaolon tarkistamiseen kannattaa käyttää `stat`-rakennetta, sillä se antaa tarkempaa tietoa tiedoston tai hakemiston ominaisuuksista. `opendir`-funktio taas avaa hakemiston ja mahdollistaa sen sisältämien tiedostojen ja hakemistojen käsittelyn. Tästä syystä onkin hyödyllistä yhdistää molemmat funktiot tarkistaaksemme hakemiston olemassaolon.

## Katso myös

- [opendir-opas C-kielellä](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [stat-opas C-kielellä](https://www.tutorialspoint.com/c_standard_library/c_function_stat.htm)