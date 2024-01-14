---
title:                "C: Merkkijonon pituuden etsiminen"
simple_title:         "Merkkijonon pituuden etsiminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi: Miksi löytää merkkijonon pituus on tärkeää

Merkkijonon pituuden laskeminen on yleinen tehtävä C-ohjelmoinnissa. Tämä voi olla hyödyllistä esimerkiksi silloin, kun haluat tarkistaa, onko merkkijono tarpeeksi pitkä ennen sen käsittelyä tai tallentamista johonkin tietorakenteeseen. Se voi myös auttaa sinua välttämään tiedon menettämistä tai virheellistä käyttöä.

## Kuinka: Esimerkkejä ja tulostesivuja käyttäen "```C ... ```" koodilohkoja

### Esimerkki 1: Löydä merkkijonon pituus

```C
#include <stdio.h>
#include <string.h>

int main()
{
    char string[100];
    int length;

    printf("Syötä merkkijono: ");
    scanf("%s", string);

    // Laske merkkijonon pituus käyttämällä strlen-funktiota
    length = strlen(string);

    printf("Merkkijonon pituus on %d merkkiä.", length);

    return 0;
}
```

#### Tulostus:

```
Syötä merkkijono: Tämä on esimerkki.
Merkkijonon pituus on 19 merkkiä.
```

### Esimerkki 2: Löydä merkkijonon pituus käyttäen omaa funktiota

```C
#include <stdio.h>

int pituus(char string[]) // Funktio, joka laskee merkkijonon pituuden
{
    int i = 0; // Alustetaan laskurimuuttuja
    while (string[i] != '\0') // Käydään läpi merkit, kunnes saavutetaan merkkijonon lopetusmerkki
    {
        i++; // Kasvatetaan laskuria jokaisella kierroksella
    }
    return i; // Palautetaan löydetty pituus
}

int main()
{
    char string[100];
    int length;

    printf("Syötä merkkijono: ");
    scanf("%s", string);

    // Laske merkkijonon pituus käyttämällä omaa pituus-funktiota
    length = pituus(string);

    printf("Merkkijonon pituus on %d merkkiä.", length);

    return 0;
}
```

#### Tulostus:

```
Syötä merkkijono: Tämä on toinen esimerkki.
Merkkijonon pituus on 25 merkkiä.
```

## Syvemmälle: Lisätietoja merkkijonon pituuden laskemisesta

Kuten voit huomata esimerkeistä, merkkijonon pituuden laskeminen on melko yksinkertaista C-ohjelmoinnissa. Voit myös huomata, että voit joko käyttää valmista `strlen`-funktiota tai luoda oman funktion merkkijonon pituuden laskemista varten.

Merkkijonon pituus lasketaan aina lopetusmerkin `\0` kohdalta. Tämä tarkoittaa, että myös välilyönnit ja muut merkit lasketaan mukaan pituuteen. Esimerkiksi merkkijonon "Hello World" pituus on 11, koska siinä on 11 merkkiä, mukaan lukien välilyönnit.

## Katso myös

- [C Strings (TutorialsPoint)](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [String Length (GeeksforGeeks)](https://www.geeksforgeeks.org/c-program-find-length-string/)