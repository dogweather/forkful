---
title:    "C: Tarkistetaan, onko hakemisto olemassa"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi
Usein ohjelmointiprojekteissa joudutaan tarkistamaan, onko tietty kansio olemassa. Tämä tarkistus on tärkeä, jotta ohjelman suoritus ei kaadu tai aiheuta virheitä, mikäli kyseistä kansioita ei löydy. Tässä blogikirjoituksessa käymme läpi kuinka tarkistus voidaan toteuttaa C-ohjelmointikielellä.

## Kuinka
Tarkistaaksesi, onko kansio olemassa C-ohjelmassa, käytetään `opendir()` funktiota. Tämä funktio avaa halutun kansion ja palauttaa osoittimen `DIR`-tyyppiseen muuttujaan. Mikäli kansiota ei ole olemassa, palauttaa `opendir()`-funktio `NULL`-arvon. Alla on esimerkki siitä, kuinka tarkistus voidaan toteuttaa käytännössä:

```C
#include <stdio.h> 
#include <dirent.h> 

int main(void) 
{ 
    DIR* dir = opendir("polku/kansioon"); 

    if (dir) { 
        printf("Kansio löytyi"); 
        closedir(dir); 
    } 
    else if (ENOENT == errno) { 
        printf("Kansiota ei löytynyt"); 
    } 
    else { 
        printf("Virhe tarkistettaessa kansiota"); 
    } 

    return 0; 
}
```

Ylläolevassa koodissa `opendir()`-funktio avaa polussa määritetyn kansion. Mikäli kansiota ei löydy, siirtyy koodi `else if`-lauseeseen, jossa tarkistetaan `errno`-muuttujan arvo. `ENOENT` tarkoittaa "ei ole". Jos `errno`-muuttujan arvo on `ENOENT`, tulostetaan, että kansiota ei löytynyt.

## Syväsyvennys
`opendir()`-funktion lisäksi on olemassa myös muita funktioita, joilla voidaan tarkistaa, onko kansio olemassa. Näitä ovat esimerkiksi `stat()` ja `access()`-funktiot. `stat()`-funktio palauttaa tiedon tiedostosta tai kansioista, kun taas `access()`-funktio tarkistaa, onko käyttäjällä oikeudet kyseiseen kansioon.

On myös tärkeää huomata, että tarkistus kansioista voi vaihdella käyttöjärjestelmästä riippuen. Esimerkiksi Windows-käyttöjärjestelmässä `opendir()`-funktio käyttää käännöstä `FindNextFile()`.

Tarkistuksen lisäksi on myös hyvä huomioida, että kansio voi olla olemassa, mutta käyttäjällä ei ole oikeuksia kyseiseen kansioon. Näin ollen virheilmoituksen sijaan voisi olla hyvä ilmoittaa, että ohjelmalla ei ole oikeuksia kyseiseen kansioon.

## Katso myös
- [C opendir() -funktio](https://www.tutorialspoint.com/c_standard_library/c_function_opendir.htm)
- [C access() -funktio](https://www.tutorialspoint.com/c_standard_library/c_function_access.htm)
- [C stat() -funktio](https://www.tutorialspoint.com/c_standard_library/c_function_stat.htm)