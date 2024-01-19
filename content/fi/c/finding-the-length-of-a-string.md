---
title:                "Merkkijonon pituuden selvittäminen"
html_title:           "Go: Merkkijonon pituuden selvittäminen"
simple_title:         "Merkkijonon pituuden selvittäminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Artikkeli: Miten löytää merkkijonon pituus C-ohjelmoinnissa: Mikä, miksi ja miten?

## Mitä & Miksi?
Merkkijonon pituuden löytäminen tarkoittaa sen määrittämistä, kuinka monta merkkiä merkkijonossa on. Ohjelmoijat tekevät tämän tietojen manipuloinnin ja valvonnan helpottamiseksi.

## Miten tehdä:
C:n standardikirjaston `strlen`-funktio auttaa löytämään merkkijonon pituuden. Katso demokoodi:

```C
#include <string.h>
#include <stdio.h>

int main() {
    char str[] = "Tervetuloa";
    printf("Merkkijonon pituus on: %zu\n", strlen(str));
    return 0;
}
```
Tämän koodin suorittaminen näyttäisi tulostuksen:

```
Merkkijonon pituus on: 10
```
## Syvempi syöksy
`strlen` on osa C:n standardikirjastoa ja sitä on käytetty laajasti merkkijonotyyppisen datan käsittelyyn ohjelmissa.

Vaihtoehtoisesti voit laskea merkkijonon pituuden itse käyttämällä loop-menetelmää, joka käy läpi merkkijonon, kunnes terminaattorimerkki `\0` löytyy.

Implementoinnissa `strlen` käy läpi merkkijonon merkki kerrallaan, kunnes se saavuttaa terminaattorimerkin, jolloin se palauttaa pituuden merkkeinä ja sulkee pois terminaattorin.

```C
size_t my_strlen(char *str) {
    char *s = str;
    while(*s)
        ++s;
    return s - str;
}
```
## Katso myös
Lisätietoja ja aiheeseen liittyvää materiaalia löytyy seuraavista lähteistä:

1. Man-sivu `strlen`: https://man7.org/linux/man-pages/man3/strlen.3.html
2. StackOverflow keskustelu merkkijonon pituuden määrittämisestä: https://stackoverflow.com/questions/4289405/efficiently-finding-the-length-of-a-string-in-c
3. C-kielen standardikirjastosta: https://en.wikipedia.org/wiki/C_standard_library