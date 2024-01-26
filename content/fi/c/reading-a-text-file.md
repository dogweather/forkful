---
title:                "Tekstitiedoston lukeminen"
date:                  2024-01-20T17:53:50.334274-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstitiedoston lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä ja Miksi?)
Tekstitiedoston lukeminen tarkoittaa tiedoston sisältämän tekstin tuomista ohjelmaasi. Ohjelmoijat tekevät tätä datan käsittelyyn, analysointiin tai ohjelmiston asetusten lataamiseen.

## How to: (Miten tehdään:)
```C
#include <stdio.h>

int main() {
    FILE *file;
    char line[100];

    file = fopen("esimerkki.txt", "r");
    if (file == NULL) {
        perror("Virhe tiedoston avaamisessa");
        return 1;
    }

    while (fgets(line, sizeof(line), file)) {
        printf("%s", line);
    }

    fclose(file);
    return 0;
}
```
Output:
```
Tässä on esimerkkitekstiä.
Toinen rivi tekstiä.
```

## Deep Dive (Syväsukellus)
Tekstitiedoston lukemisen historia alkaa jo varhaisista käyttöjärjestelmistä, kun kaikki tieto tallennettiin ja käsiteltiin tekstimuodossa. C-kielen standardikirjasto tarjoaa funktiot `fopen`, `fgets`, `fread`, jne. tiedostojen käsittelyyn. Vaihtoehtoisia tapoja lukea tiedostoja ovat esimerkiksi `mmap`, kolmannen osapuolen kirjastot tai komentorivipohjaiset ratkaisut. `fgets` lukee tiedoston rivi riviltä, kun taas `fread` voi lukea suurempia datablokkeja. Tietoturvan kannalta on tärkeää muistaa tarkistaa tiedoston avaaminen ja käsitellä virheitä oikein, ettei luotaisi haavoittuvuuksia.

## See Also (Katso Myös)
- C Standard Library documentation: https://en.cppreference.com/w/c/io
- GNU C Library Manual: https://www.gnu.org/software/libc/manual/html_node/Opening-Streams.html
- POSIX `mmap`: https://man7.org/linux/man-pages/man2/mmap.2.html
