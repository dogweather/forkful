---
title:                "Alimerkkijonojen poiminta"
html_title:           "Gleam: Alimerkkijonojen poiminta"
simple_title:         "Alimerkkijonojen poiminta"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

# **Substringien erottaminen C-ohjelmoinnissa**

## Mitä ja miksi?

Substringien erottaminen on prosessi, jossa valitaan ja otetaan osa merkkijonosta. Tarvitsemme tätä erityisesti silloin, kun haluamme käsitellä vain osaa tiedosta, jonka merkkijono sisältää.

## Kuinka tehdä:

Alla on perusohje substringien erottamiseen C-ohjelmoinnissa. Käytämme `strncpy`-funktiota.

```C
#include <string.h>
#include <stdio.h>

int main() {
    char source[] = "Tervetuloa Suomi!";
    char target[10];

    strncpy(target, source, 9);
    target[9] = '\0'; // terminating null byte

    printf("Source string: %s\n", source);
    printf("Extracted substring: %s\n", target);

    return 0;
}
```

Tämä ohjelma palauttaa seuraavat tulosteet:

```
Source string: Tervetuloa Suomi!
Extracted substring: Tervetuloa
```

## Syvä sukellus

### Historia
`strncpy` on ikivanha funktio, joka on tullut meille C:n standardikirjastosta. Se on ollut valtavasti hyödyllinen tekstikäsittelyn liialliseen monimutkaistamiseen liittyvissä ongelmissa.

### Vaihtoehdot
On olemassa muita funktioita, joita voit käyttää myös substringien kanssa. Esimerkiksi:

- `sscanf`
- `strpbrk`
- `strtok`

Sopivan funktion valinta riippuu siitä, mitä tarkalleen ottaen haluat saavuttaa.

### Toteutus
`strncpy` kopioi N merkkiä alkuperäisestä merkkijonosta kohteeseen. Tämä tuo esille sen, että meidän on varmistettava, että kohdassa on tarpeeksi tilaa tietojen tallentamiseksi. Myös päättelevä null-merkki on lisättävä manuaalisesti.

## Katso myös

- [C Library - <string.h>](https://www.tutorialspoint.com/c_standard_library/string_h.htm) TutorialsPointissa.
- [Function strncpy() in C](https://www.geeksforgeeks.org/function-strncpy-in-c/) GeeksforGeeksissä.
- [How to take a substring in C](https://stackoverflow.com/questions/4214314/get-a-substring-of-a-char) StackOverflow'ssa.