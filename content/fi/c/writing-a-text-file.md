---
title:    "C: Tekstitiedoston kirjoittaminen"
keywords: ["C"]
---

{{< edit_this_page >}}

## Miksi

Tekstien kirjoittaminen on yksi tärkeimmistä ohjelmoinnin osa-alueista. Se mahdollistaa tiedon tallentamisen ja jakamisen tarkoituksenmukaisesti ja järjestelmällisesti. Tekstien kirjoittaminen on myös olennainen osa tiedonkäsittelyä, joka on tärkeä taito jokaiselle ohjelmoijalle.

## Kuinka

Kirjoittamalla tekstiä C-ohjelmoinnissa, voit käyttää "fprintf" -funktiota ja määrittää tiedoston nimen sekä tulostettavan tekstin muuttujana. Tämän jälkeen voit käyttää "fopen" -funktiota avataksesi tiedoston ja "fprintf" -funktiota kirjoittaaksesi tekstiä siihen. Lopuksi muista sulkea tiedosto "fclose" -funktiolla.

```
#include <stdio.h>

int main()
{
    FILE *tiedosto;
    char nimi[50];
    char teksti[100];

    printf("Syötä tiedoston nimi: "); // pyydetään käyttäjältä tiedoston nimi
    fgets(nimi, 50, stdin);

    printf("Syötä tekstiä: "); // pyydetään käyttäjältä tekstiä
    fgets(teksti, 100, stdin);

    tiedosto = fopen(nimi, "w"); // avataan tiedosto kirjoittamista varten

    fprintf(tiedosto, "%s", teksti); // kirjoitetaan teksti tiedostoon

    fclose(tiedosto); // suljetaan tiedosto

    printf("Tiedosto %s kirjoitettu onnistuneesti.\n", nimi); // vahvistetaan tiedoston kirjoittaminen
}
```

### Tuloste:

```
Syötä tiedoston nimi: teksti.txt
Syötä tekstiä: Tämä on esimerkki tekstistä.

Tiedosto teksti.txt kirjoitettu onnistuneesti.
```

## Syvemmälle

Tekstin kirjoittaminen C-ohjelmoinnissa vaatii yksityiskohtaista tietoa tiedostojen käsittelystä ja tiedon tallentamisesta. On tärkeää ymmärtää, että tiedoston avaaminen kirjoittamista varten voi korvata olemassa olevan tiedoston ja kirjoituksen lisäksi myös lisätä sitä. Muista myös aina sulkea tiedosto, kun se ei ole enää tarpeen.

## Katso myös

- [fprintf function in C](https://www.geeksforgeeks.org/fprintf-in-c/)
- [fopen function in C](https://www.geeksforgeeks.org/fopen-function-in-c/)
- [fclose function in C](https://www.geeksforgeeks.org/fclose-in-c/)