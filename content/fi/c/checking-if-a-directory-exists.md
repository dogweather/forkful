---
title:    "C: Tarkistetaan, onko kansio olemassa"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi tarkistaa hakemiston olemassaolo?

Hakemistojen olemassaolon tarkistaminen on tärkeä osa C-ohjelmointia, sillä se varmistaa, että tiedostojen käsittely sujuu suunnitellusti. Kun tarkistat ensin, onko hakemisto olemassa, voit välttää mahdolliset virheet ohjelman suorituksessa ja optimoida suoritusaikaa.

## Miten tarkistaa hakemiston olemassaolo

Tarkistaaksesi, onko hakemisto olemassa C-kielellä, käytä funktiota ```opendir ()``` ja tarkista sen palauttaman arvon avulla. Jos palautusarvo on NULL, hakemistoa ei ole olemassa. Alla on esimerkki koodista, joka tarkistaa "testi" nimisen hakemiston olemassaolon:

```
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>

int main() {
    
    // Avataan hakemisto "testi" ja tarkistetaan sen olemassaolo
    DIR *dir = opendir("testi");
    if (dir == NULL)
        printf("Hakemistoa ei löydy \n");
    else
        printf("Hakemisto löydetty \n");
        
    return 0;
}
```

Tulostus tältä koodilta olisi "Hakemistoa ei löydy", jos "testi" nimistä hakemistoa ei löydy, tai "Hakemisto löydetty", jos hakemisto on olemassa.

## Syventyminen hakemiston olemassaolon tarkistamiseen

Tarkemmin sanottuna, ```opendir()``` funktio avaa hakemiston ja palauttaa sen osoittimen tai NULL, jos hakemistoa ei löydy. Jotta voisit käyttää muita hakemiston toimintoja, kuten ```readdir()```, sinun tulee ensin tarkistaa, onko hakemisto voimassa.

On myös hyvä huomata, että ```opendir()``` funktiolla on merkittävä vaikutus suoritusaikaan. Jos tarvitset vain tarkistaa hakemiston olemassaolon, on parempi käyttää ```access()``` funktiota, joka suorittaa paljon nopeammin. Tämä funktio tarkistaa, onko tietyllä polulla sijaitseva hakemisto voimassa. Alla on esimerkki:

```
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <dirent.h>

int main() {
    
    // Tarkistetaan "testi" hakemisto by polku
    if (access("testi", F_OK) != -1)
        printf("Hakemisto löydetty \n");
    else
        printf("Hakemistoa ei löydy \n");
        
    return 0;
}
```

## Katso myös

- [Dirent.h - C-käyttöliittymä](https://www.tutorialspoint.com/c_standard_library/dirent_h.htm)
- [DIR rakenne - C käyttöliittymä](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [Access() funktio - C käyttöliittymä](https://www.tutorialspoint.com/c_standard_library/c_function_access.htm)