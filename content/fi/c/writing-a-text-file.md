---
title:                "Tekstitiedoston kirjoittaminen"
date:                  2024-01-19
html_title:           "Arduino: Tekstitiedoston kirjoittaminen"
simple_title:         "Tekstitiedoston kirjoittaminen"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä ja Miksi?
Kirjoitamme tekstifileihin tallentaaksemme dataa pysyvästi. Koodarit tekevät tämän tiedon lokiin kirjaamiseksi, konfiguraatioiden tallentamiseksi tai käyttäjän tuoton säilyttämiseksi.

## How to: - Kuinka tehdä:
```C
#include <stdio.h>

int main() {
    FILE *file;
    file = fopen("esimerkki.txt", "w"); // Avaa tiedoston kirjoitusta varten

    if (file == NULL) {
        perror("fopen");
        return -1;
    }

    fprintf(file, "Hei Suomi!\n"); // Kirjoittaa tiedostoon

    fclose(file); // Sulkee tiedoston
    return 0;
}
```
Tuloste: Tiedostoon `esimerkki.txt` tallentuu teksti "Hei Suomi!".

## Deep Dive - Syväsukellus:
### Historiallinen konteksti:
Tekstitiedostojen kirjoittaminen on ollut osa C-kielen standardikirjastoa alusta alkaen. UNIX-ympäristöissä tämä on yleinen tapa kommunikoida prosessien välillä ja tallentaa asetuksia.

### Vaihtoehdot:
Voit myös käyttää funktioita `write`, `fwrite` tai moderneja lähestymistapoja kuten stream-objekteja C++:ssa.

### Toteutuksen yksityiskohdat:
C99-standardista alkaen "r+", "w+", ja "a+" moodit mahdollistavat lukemisen ja kirjoittamisen samassa avauksessa. Muista varmistaa tiedoston sulku `fclose`-funktiolla välttääksesi muistivuotoja.

## See Also - Katso Myös:
- C Standard Library, Input/Output: http://en.cppreference.com/w/c/io
- Linux Manual page for `fopen(3)`: https://man7.org/linux/man-pages/man3/fopen.3.html
- C File I/O Guide: https://www.tutorialspoint.com/cprogramming/c_file_io.htm
