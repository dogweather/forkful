---
title:                "C: Satunnaisten numeroiden generointi."
simple_title:         "Satunnaisten numeroiden generointi."
programming_language: "C"
category:             "C"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä, erityisesti kun kyseessä on pelit tai tietokonesimulaatiot, tarvitsemme satunnaisia lukuja. Näitä lukuja kutsutaan pseudosatunnaisiksi, koska ne eivät ole täysin satunnaisia, mutta ne voivat luoda tarvittavan simulaation tai tunnelman.

## Kuinka

C-ohjelmointikielessä on sisäänrakennettu rand-funktio, joka tuottaa pseudosatunnaisen luvun. Sen käyttö on yksinkertaista:

```C
#include <stdio.h>
#include <stdlib.h>

int main()
{
    // Tulostaa 10 satunnaista lukua väliltä 0-99
    for (int i = 0; i < 10; i++)
    {
        printf("%d\n", rand() % 100); 
    }

    return 0;
}
```

Esimerkkitulostus:

```
36
79
50
5
21
94
87
29
12
60
```

## Syventymistä

Rand-funktio käyttää taustallaan matemaattista algoritmia, joka luo näennäisesti satunnaisia numeroita. Tämä algoritmi perustuu usein johonkin muuttuvaan tietokoneen sisäiseen arvoon, kuten kellonaikaan, mikä tekee luvuista vaihtelevampia. Jotkin C-kirjastot, kuten <stdlib.h>, tarjoavat myös muita satunnaislukujen generaattoreita, joilla on erilaisia ominaisuuksia ja toimintatapoja.

## Katso myös

- [C-kielen opas](https://www.c-howto.de/tutorial/einfuehrung/c/)
- [C-koodiesimerkkejä](https://github.com/TheAlgorithms/C)
- [C-kirjastot ja funktiot](https://en.cppreference.com/w/c/header)