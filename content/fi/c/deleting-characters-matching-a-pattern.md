---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Tervetuloa: Miten Poistaa Merkkejä Vastaavasta Mallista C-ohjelmoinnissa

## Mitä & Miksi?
Poistaminen merkkejä vastaavasta mallista tarkoittaa tiettyjen merkkien (mallin) poistamista merkkijonosta. Tätä käytetään datan siistimiseen ja tarpeettoman tiedon poistamiseen.

## Kuinka se tapahtuu:
Alla on esimerkki siitä, kuinka tämä tehdään.

```C
#include <stdio.h>
#include <string.h>

void delete_pattern(char *str, const char *pattern) {
    char *p = str;
    while (*str) {
        if (strchr(pattern, *str)) {
            str++;
        } else {
            *p++ = *str++;
        }
    }
    *p = '\0';
}

int main() {
    char str[] = "Hello, World!";
    delete_pattern(str, " World!");
    printf("%s\n", str); // tulostaa: Hello,
    return 0;
}
```

Mallin " World!" merkkijono poistetaan alkuperäisestä merkkijonosta "Hello, World!".

## Syvällisempi tarkastelu
Merkkien poistaminen malleilla on ollut osa C-kieltä alusta asti. Tämän ratkaisemiseksi on olemassa useita vaihtoehtoisia menetelmiä, mutta tässä artikkelissa esitelty menetelmä on yksinkertaisin ja tehokkain.

Toteutuksen osalta funktio `delete_pattern` käy läpi merkkijonon ja verrata sitä malliin. Jos merkki on mallissa, se ohitetaan. Muussa tapauksessa, se kopioidaan uuteen merkkijonoon `p`. 

## Katso myös
1. Stack Overflow: Removing all occurrences of a character in a string - [Stackoverflow linkki](https://stackoverflow.com/questions/5457608/how-to-remove-the-character-at-a-given-index-from-a-string-in-c)
2. Tutorialspoint: String handling functions in C - [Tutorialspoint linkki](https://www.tutorialspoint.com/c_standard_library/c_function_strchr.htm)

Onnea ohjelmointiprojekteillesi! Koodauksen maailma odottaa.