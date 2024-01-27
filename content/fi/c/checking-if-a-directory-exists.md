---
title:                "Tarkistetaan, onko hakemisto olemassa"
date:                  2024-01-19
html_title:           "C: Tarkistetaan, onko hakemisto olemassa"
simple_title:         "Tarkistetaan, onko hakemisto olemassa"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Tarkistetaan, onko hakemisto olemassa, koska tiedostoon kirjoittaminen tai siitä lukeminen voi epäonnistua, jos hakemistoa ei ole. Tämä tarkistus auttaa välttämään virheet, jotka johtuvat olemattomista poluista.

## How to: (Kuinka tehdä:)
```C
#include <sys/stat.h>
#include <stdio.h>

int directory_exists(const char *path) {
    struct stat statbuf;
    // Onnistuuko hakemiston tietojen haku
    if (stat(path, &statbuf) != 0){
        return 0; // Ei, palauta 0
    }
    // Onko kyseessä hakemisto
    return S_ISDIR(statbuf.st_mode);
}

int main() {
    const char *path = "/path/to/directory";

    if(directory_exists(path)) {
        printf("Hakemisto on olemassa.\n");
    } else {
        printf("Hakemistoa ei ole olemassa.\n");
    }

    return 0;
}
```
Esimerkkitulostus:
```
Hakemisto on olemassa.
```
tai
```
Hakemistoa ei ole olemassa.
```

## Deep Dive (Syväsukellus):
C:ssä hakemiston olemassaolon tarkistus tapahtuu usein `stat` funktion avulla. Sitä on käytetty Unix-pohjaisissa järjestelmissä jo vuosikymmenet. Vaihtoehtoinen tapa on käyttää `opendir` ja `closedir` funktioita. Nämä toiminnot voivat paljastaa lisätietoa hakemistosta, mutta ovat hitaampia. Käytettäessä `stat`, tarkistetaan että `st_mode` kentän arvo vastaa hakemistoa `S_ISDIR` makron avulla.

Hakemiston olemassaolon tarkistaminen on tärkeä varotoimi monissa ohjelmissa. Esimerkiksi, ennen tiedoston luontia hakemistoon, kannattaa varmistaa että hakemisto todella on olemassa. Näin vältytään virheiltä, jotka saattaisivat kaataa ohjelman tai jäädä huomaamatta pitemmäksi aikaa.

## See Also (Katso Myös):
- C Standard Library documentation (C-standardikirjaston dokumentaatio): https://en.cppreference.com/w/c
- GNU C Library manual (GNU C-kirjaston käsikirja): https://www.gnu.org/software/libc/manual/
- POSIX standard (POSIX-standardeja): http://pubs.opengroup.org/onlinepubs/9699919799/
