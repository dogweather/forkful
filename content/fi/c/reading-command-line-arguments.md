---
title:    "C: Komentoriviparametrien lukeminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Miksi haluat lukea komentoriviargumentteja C-ohjelmoinnissa? Komentoriviargumentteja käytetään ohjelman parametrien antamiseen ennen sen suorittamista, mikä tekee ohjelman käytöstä monipuolisempaa ja joustavampaa. Tämä artikkeli auttaa sinua ymmärtämään miten komentoriviargumentteja käytetään C-ohjelmoinnissa.

## Miten

```C
#include <stdio.h>

int main(int argc, char *argv[]){
    // Printtaa kaikki komentoriviargumentit
    for(int i = 0; i < argc; i++){
        printf("Argumentti %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

**Esimerkki tulostus:**

```
$ ./argumentit koirat linnut
Argumentti 0: ./argumentit
Argumentti 1: koirat
Argumentti 2: linnut
```

Kuten esimerkistä näemme, komentoriviargumentit tallennetaan `argv` taulukkoon, ja ohjelman suorittamisen yhteydessä voimme käsitellä niitä haluamallamme tavalla.

## Syvällistä tietoa

Komentoriviargumentteja voidaan käyttää myös komentoriviltä syötettävien tietojen lukemiseen, esimerkiksi tiedoston nimen tai parametrien arvojen välittämiseen ohjelmalle. `argc` muuttuja kertoo kuinka monta argumenttia on annettu, ja `argv` taulukossa olevat argumentit voidaan muuttaa halutuiksi tietotyypeiksi, kuten kokonaisluvuiksi tai merkkijonoiksi.

## Katso myös

- [C-ohjelmointi - Wikikirjasto](https://fi.wikibooks.org/wiki/C-ohjelmointi)
- [Komentoriviparametrit C-kielessä - GeeksforGeeks](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [C-kielen komentoriviargumenttien perusteet - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_command_line_arguments.htm)