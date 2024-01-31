---
title:                "Koodin järjestäminen funktioihin"
date:                  2024-01-26T01:10:17.795257-07:00
model:                 gpt-4-1106-preview
simple_title:         "Koodin järjestäminen funktioihin"

category:             "C"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Koodin järjestäminen funktioihin tarkoittaa koodin pilkkomista uudelleenkäytettäviin lohkoihin, jotka suorittavat tiettyjä tehtäviä. Se tekee koodista helpommin luettavaa, vianetsintää ja ylläpitoa.

## Kuinka:
Otetaan yksinkertainen esimerkki: sanotaan, että haluat laskea kahden luvun summan useita kertoja.

Ilman funktioita:
```C
#include <stdio.h>

int main() {
    int summa1 = 5 + 3;
    printf("Summa1: %d\n", summa1);
    
    int summa2 = 2 + 8;
    printf("Summa2: %d\n", summa2);
    
    // Lisää yhteenlaskuja täällä...
    
    return 0;
}
```

Funktioiden kanssa:
```C
#include <stdio.h>

int lisaa(int a, int b) {
    return a + b;
}

int main() {
    int summa1 = lisaa(5, 3);
    printf("Summa1: %d\n", summa1);
    
    int summa2 = lisaa(2, 8);
    printf("Summa2: %d\n", summa2);
    
    // Käytä lisaa()-funktiota lisää yhteenlaskuja varten...
    
    return 0;
}
```

Tulostus:
```
Summa1: 8
Summa2: 10
```

## Syväluotaus
Ennen kuin C:ssä oli funktioita, ohjelmointi tehtiin usein lineaarisesti, paljon kuin reseptiä seuraten. Mutta kun ohjelmat kasvoivat, koodin toistaminen muodostui ongelmaksi. Funktiot olivat ratkaisu – ne mahdollistivat saman koodilohkon suorittamisen ohjelman eri osista uudelleenkirjoittamatta sitä joka kerta. Tämä ei ainoastaan säästä tilaa vaan myös aikaa päivitysten tekemisessä: muuta funktiota yhdessä paikassa, ja jokainen koodinosa, joka sitä käyttää, saa päivityksen.

Vaihtoehtoja funktioille voisivat olla sisäinen koodi, makrot tai kopioi-ja-liitä -ohjelmointi, mutta nämä voivat johtaa turpeeseen, virhealttiiseen ja huoltovaikeaan koodiin. Funktionaalit päinvastoin kapseloivat toiminnallisuutta, määrittelevät selkeät rajapinnat ja voivat vähentää sivuvaikutuksia asianmukaisen käyttöalueen avulla.

Kun toteutat funktioita, harkitse muutamaa yksityiskohtaa: ensinnäkin, yritä saada ne tekemään vain yksi asia – tätä kutsutaan Yksittäisen Vastuun Periaatteeksi. Toiseksi, nimet ovat tärkeitä – valitse kuvaavat nimet funktioille ja niiden parametreille, jotta koodisi on itsestään dokumentoiva.

## Katso myös
Lisätietoa C:n funktioista löytyy näistä:

- C Standard Library viite: https://en.cppreference.com/w/c/header
- C-ohjelmointi: Nykyaikainen lähestymistapa, kirjoittanut K.N. King: Kirja, josta löytyy syväluotaus funktioista.
- Learn-C.org: Funktioiden osio: https://www.learn-c.org/en/Functions
