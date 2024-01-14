---
title:                "C: Mallin mukaisten merkkien poistaminen"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Poistamaan merkkejä mallin mukaan vastaavilta merkkijonoilta:

`cd test`
```C
char str[] = "tervetuloa";
int i, j;

// Poistetaan merkit, jotka vastaavat merkkijonoa "velo"
for (i = 0; str[i] != '\0'; ++i) {
    while (!(str[i] == 'v' 
          && str[i+1] == 'e' 
          && str[i+2] == 'l' 
          && str[i+3] == 'o')) {
        str[j++] = str[i];
    }
    i = i + 3;
}
str[j] = '\0';

printf("%s", str);

// Tulostaa "tetua" 
```

## Miten tehdä

 Jotta voit poistaa merkkejä vastaavilta merkkijonoilta C-ohjelmointikielessä, sinun on käytettävä silmukkaa ja vertailtava jokaisen kirjaimen avulla, onko se osa haluttua merkkijonoa. Tämän jälkeen voit poistaa merkin käyttämällä toista silmukkaa ja siirtämällä seuraavat kirjaimet korvaamaan poistetun merkin. Lopuksi, voit asettaa loppuun nollamerkin varmistaaksesi, että merkkijono on oikeassa muodossa ilman poistettuja merkkejä.

## Syvällinen sukellus

Mikäli haluat poistaa merkkejä erilaisilta malleilta tai käyttää monimutkaisempia ehtoja, voit käyttää C-ohjelmointikielessä tarjolla olevia merkeistötyökaluja. Esimerkiksi `isalpha()`-funktio voi auttaa tunnistamaan, onko kyseessä kirjain, ja sitä voidaan käyttää ehtona poistaessasi merkkejä.

## Katso myös

- [C-ohjelmointikielen dokumentaatio](https://www.tutorialspoint.com/cprogramming/)
- [C-merkkijonojen manipulointi](https://www.geeksforgeeks.org/c-programming-language/)
- [Merkistöfunktiot C-ohjelmointikielessä](https://www.programiz.com/c-programming/library-function/ctype.h)