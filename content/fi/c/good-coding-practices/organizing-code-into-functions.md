---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 17:59:19.269579-07:00
description: "Miten: C-kieless\xE4 funktio ilmaistaan paluutyypill\xE4, nimell\xE4\
  \ ja parametreilla (jos niit\xE4 on), jonka j\xE4lkeen seuraa koodilohko. Aloittakaamme\u2026"
lastmod: '2024-03-13T22:44:57.046276-06:00'
model: gpt-4-0125-preview
summary: "C-kieless\xE4 funktio ilmaistaan paluutyypill\xE4, nimell\xE4 ja parametreilla\
  \ (jos niit\xE4 on), jonka j\xE4lkeen seuraa koodilohko."
title: "Koodin j\xE4rjest\xE4minen funktioihin"
weight: 18
---

## Miten:
C-kielessä funktio ilmaistaan paluutyypillä, nimellä ja parametreilla (jos niitä on), jonka jälkeen seuraa koodilohko. Aloittakaamme yksinkertaisella esimerkillä: funktiolla, joka lisää kaksi kokonaislukua.

```c
#include <stdio.h>

// Funktiodeklaraatio
int add(int a, int b);

int main() {
  int summa = add(5, 3);
  printf("Summa on: %d\n", summa);
  return 0;
}

// Funktion määrittely
int add(int a, int b) {
  return a + b;
}
```

Tuloste:
```
Summa on: 8
```

Katsotaan nyt monimutkaisempaa esimerkkiä, joka sisältää mukautetun tietotyypin. Tämä funktio laskee suorakulmion pinta-alan.

```c
#include <stdio.h>

// Määritellään rakenne suorakulmiolle
typedef struct {
  int leveys;
  int korkeus;
} Suorakulmio;

// Funktio suorakulmion pinta-alan laskemiseksi
int laskeAla(Suorakulmio rect) {
  return rect.leveys * rect.korkeus;
}

int main() {
  Suorakulmio omaRect = {5, 10};
  int ala = laskeAla(omaRect);
  printf("Suorakulmion pinta-ala on: %d\n", ala);
  return 0;
}
```

Tuloste:
```
Suorakulmion pinta-ala on: 50
```

## Syväsukellus
Funktioiden käsite C-kielessä, peritty aikaisemmista ohjelmointikäytänteistä, on olennainen rakenteellisessa ohjelmoinnissa. Funktiot mahdollistavat yksityiskohtien abstrahoinnin, monimutkaisuuden hallinnan ja koodin loogisen järjestämisen. Sen alusta lähtien, funktio on ollut keskeinen rakennuspalikka C-kielessä, vaikuttaen lukuisiin muihin kieliin.

Ohjelmointiparadigmojen kehittymisen myötä, vaihtoehtoiset lähestymistavat kuten olio-ohjelmointi (OOP) kielissä kuten C++ ja Java, ovat laajentaneet funktioiden käsitettä objekteihin liittyvillä metodeilla. Vaikka C ei suoraan tue OOP:tä, on mahdollista matkia olio-ohjelmointimalleja huolellisesti rakennettujen funktioiden ja datan avulla.

Nykyohjelmoinnissa funktiot ovat edelleen keskeisiä, mutta kääntäjän optimointien ja kielen ominaisuuksien kehittyessä painopiste saattaa siirtyä inline-funktioihin ja malleihin C++:ssa tai lambdoihin kielissä kuten Python ja JavaScript. Nämä tarjoavat enemmän joustavuutta ja usein tiiviimmän syntaksin saman modulaarisuuden ja uudelleenkäytettävyyden saavuttamiseksi. Kuitenkin, koodin järjestämisen perusperiaatteet funktioihin C-kielessä ovat yleisesti sovellettavissa ja muodostavat tehokkaan ja vaikuttavan ohjelmistokehityksen perustan.
