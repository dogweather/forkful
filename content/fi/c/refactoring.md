---
title:                "Koodin refaktorointi"
date:                  2024-01-26T01:16:56.499136-07:00
model:                 gpt-4-0125-preview
simple_title:         "Koodin refaktorointi"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/refactoring.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Refaktorointi on prosessi, jossa olemassa olevaa tietokonekoodia uudelleenjärjestetään muuttamatta sen ulkoista käyttäytymistä. Ohjelmoijat tekevät sitä parantaakseen luettavuutta, vähentääkseen monimutkaisuutta tai tehdäkseen koodista ylläpidettävämpää ja skaalautuvampaa, mikä voi säästää kasapäin aikaa ja päänsärkyä tulevaisuudessa.

## Kuinka:
Ehostetaan hieman koodia. Kuvittele, että sinulla on funktio, joka laskee kokonaislukujen keskiarvon taulukossa. Ensivilkaisulla se on hieman sotkuinen.

**Ennen refaktorointia:**
```C
#include <stdio.h>

double calculateStuff(int numbers[], int size) {
    int sum = 0;
    double average;
    for(int i = 0; i < size; sum += numbers[i++]); // Summataan for-silmukan ehdon yhteydessä, ai ai!
    average = (double)sum / size;
    palauta average;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Keskiarvo: %f\n", calculateStuff(array, length));

    return 0;
}
```

**Refaktoroinnin jälkeen:**
```C
#include <stdio.h>

int calculateSum(const int numbers[], int size) {
    int sum = 0;
    for (int i = 0; i < size; ++i) {
        sum += numbers[i];
    }
    return sum;
}

double calculateAverage(const int numbers[], int size) {
    int sum = calculateSum(numbers, size);
    return (double)sum / size;
}

int main() {
    int array[] = {23, 42, 57, 16, 5, 102};
    int length = sizeof(array) / sizeof(array[0]);
    printf("Keskiarvo: %f\n", calculateAverage(array, length));
    return 0;
}
```
Vaikka kyseessä on yksinkertainen esimerkki, voit nähdä, kuinka funktion jakaminen tekee koodista puhtaampaa ja ylläpidettävämpää. Kullakin funktiolla on nyt yksi vastuualue – tärkeä periaate puhtaassa koodissa.

## Syväsukellus
Termin "refaktorointi" popularisoi myöhään 90-luvulla, erityisesti Martin Fowlerin kirjan "Refaktorointi: Olemassa olevan koodin suunnittelun parantaminen" julkaisu. Refaktorointi ei tarkoita virheiden korjaamista tai uusien ominaisuuksien lisäämistä, vaan pikemminkin koodin rakenteen parantamista.

On monia hienoja refaktorointityökaluja ja IDEjä (Integroidut Kehitysympäristöt), kuten CLion C:lle ja C++:lle, jotka auttavat automatisoimaan prosessin, mutta ymmärrys siitä, mitä tapahtuu kulissien takana, pysyy olennaisena.

Vaihtoehtoja refaktoroinnille voivat sisältää koodin kirjoittamisen alusta alkaen uudelleen (riskialtista ja usein tarpeetonta) tai teknisen velan kanssa elämisen (joka voi olla kalliimpaa pitkällä aikavälillä). Toteutuksen yksityiskohdat vaihtelevat projektista toiseen, mutta yleisiä refaktorointeja sisältävät muuttujien uudelleennimeämisen selkeyden vuoksi, suurten funktioiden jakamisen pienempiin ja taikasanojen korvaaminen nimetyillä vakioilla.

Lisäksi, mallit kuten DRY (Don't Repeat Yourself) ja SOLID-periaatteet voivat ohjata refaktorointimatkaasi, tavoitteena koodikanta, joka on helpompi testata, ymmärtää ja tehdä yhteistyötä.

## Katso myös
Sukellus refaktoroinnin syvään mereen, katso:

- Martin Fowlerin kotisivu: https://martinfowler.com/ sisältää aarreaitan artikkeleita ja resursseja refaktoroinnista ja ohjelmistosuunnittelusta.
- Refactoring.com: https://refactoring.com/ tarjoaa esimerkkejä ja luetteloita refaktorointitekniikoista.
- Kirja "Refaktorointi": Pidetään refaktoroinnin raamattuna, sen lukeminen antaa sinulle täydellisen kuvan metodologiasta.
- "Clean Code: A Handbook of Agile Software Craftsmanship" Robert C. Martinilta, joka keskustelee helppotajuisen ja ylläpidettävän koodin kirjoittamisesta.
