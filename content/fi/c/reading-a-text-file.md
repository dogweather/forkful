---
title:                "C: Tiedostosta lukeminen"
simple_title:         "Tiedostosta lukeminen"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Haluatko oppia lukemaan tekstitiedostoja omassa C-ohjelmoinnissasi? Ei hätää, tässä blogikirjoituksessa käydään läpi helppoja esimerkkejä ja syvällisempää tietoa tekstitiedostojen lukemisesta.

## Kuinka

Käsittelemme tässä kirjoituksessa tekstitiedoston lukemisen kahdella eri tavalla, käyttäen `scanf` ja `fscanf` funktioita. Näiden lisäksi käytämme myös `while`-silmukkaa tiedoston lukemiseen. Alla on koodiesimerkki, jossa käytämme `scanf` funktiota lukemaan tekstitiedostosta yhden rivin kerrallaan:

```C
#include <stdio.h>
#define MAX_LEN 100

int main(){
    FILE *file_ptr = fopen("tiedosto.txt", "r");
    char line[MAX_LEN];
    
    while (fscanf(file_ptr, "%[^\n]", line) == 1){
        printf("%s\n", line);
    }
    
    fclose(file_ptr);
    
    return 0;
}
```

Tämän esimerkin avulla voit lukea tekstitiedostosta yhden rivin kerrallaan, kunnes tiedosto loppuu. `fscanf` funktiolla voit määrittää, miten paljon haluat lukea tiedostosta kerrallaan. Voit myös käyttää muita muotoilumerkkejä, kuten `%d` tai `%f`, jos haluat lukea tiettyjä tyyppejä tiedostosta. 

## Syvempää tietoa

On tärkeää muistaa, että `fscanf` funktio lukee tiedoston etenemisen mukaan, joten sinun täytyy olla varovainen, ettei tiedoston lukeminen lopu ennen kuin olet käynyt läpi kaikki rivit. Voit myös käyttää `fgets` funktiota, joka lukee tiedoston rivin kerrallaan ja tallentaa sen merkkijonoon. Voit sitten käyttää `sscanf` funktiota lukeaksesi tiettyjä tietoja merkkijonosta.

Tärkeintä on, että muistat aina sulkea tiedosto `fclose` funktiolla, kun olet lopettanut sen lukemisen.

## Katso myös

Katso lisätietoja tekstitiedostojen lukemisesta C-kielellä seuraavien linkkien kautta:

- [GeeksforGeeks: Basics of File Handling in C](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [Tutorials Point: C - File I/O](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C-ohjelmointikieli: Tiedostojen lukeminen ja kirjoittaminen](https://www.c-ohjelmointikieli.fi/toinenluku/tiedostot.html)