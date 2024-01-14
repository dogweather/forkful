---
title:    "C: Satunnaislukujen luominen"
keywords: ["C"]
---

{{< edit_this_page >}}

# Miksi
Monet ohjelman kehittäjät saattavat tarvita tarvetta luoda satunnaisia numeroita ohjelmiaan varten, kuten pelisovelluksia tai erilaisia simulaatioita varten. Tämä artikkeli kertoo, kuinka helposti voit luoda satunnaisgeneraattoreita omassa C-ohjelmassasi.

# Kuinka
Käyttämällä C-ohjelmoijien standardikirjastoa <stdlib.h>, voit luoda satunnaislukuja helposti käyttämällä funktion ```rand()```. Ohessa näet yksinkertaisen esimerkin, jossa koodin avulla luodaan 10 satunnaista kokonaislukua väliltä 1-100 ja tulostetaan ne näytölle.

```C
#include <stdio.h>
#include <stdlib.h>

int main(void)
{
    // Luodaan satunnaisgeneraattori
    srand(time(NULL));

    // Luodaan 10 satunnaista kokonaislukua ja tulostetaan ne näytölle
    for (int i = 0; i < 10; i++)
    {
        int random = rand() % 100 + 1;
        printf("%d ", random);
    }

    return 0;
}

// Output:
// 44 88 21 72 90 55 17 38 75 9
```

Tässä esimerkissä käytetään myös ```srand()```-funktiota, joka asettaa satunnaislukugeneraattorin aloitusarvoksi ajanhetken, jolloin ohjelma käynnistyy. Tämä mahdollistaa jokaisen suorituskerran jälkeen uusien erilaisten satunnaislukujen luomisen.

# Syvällisemmin
Satunnaislukujen luominen ohjelmassa perustuu pseudosatunnaislukugeneraattoreihin, jotka ovat matemaattisia algoritmeja, jotka tuottavat suuren joukon numeroita, jotka näyttävät satunnaisilta. On tärkeää huomata, että nämä luvut eivät ole täysin satunnaisia, mutta ne ovat tarpeeksi lähellä sitä, jotta ne ovat hyödyllisiä monissa sovelluksissa. Satunnaislukugeneraattoreiden algoritmit perustuvat yleensä alkuperäiseen siemenlukuun, joka on aina sama, joten ohjelman suoritettaessa sama alkuperäinen siemen tuottaa aina saman satunnaislukujen sarjan. Tämän vuoksi ```srand()```-funktion käyttäminen ajanhetken perusteella on suositeltavaa, jotta satunnaislukujen sarja vaihtelisi suorituksen kullekin kerralle.

# Katso myös
- [stdlib.h dokumentaatio (suomeksi)](https://cplusplus.com/reference/cstdlib/)
- [Satunnaislukujen pseudosatunnaislukugeneraattorit (suomeksi)](https://fi.wikipedia.org/wiki/Pseudosatunnaislukugeneraattori)
- [Ajanhetken käyttäminen pseudosatunnaislukujen aloitusarvona (englanniksi)](https://www.geeksforgeeks.org/rand-and-srand-in-ccpp/)