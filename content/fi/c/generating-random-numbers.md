---
title:    "C: Sattumanvaraisten numeroiden luominen"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Miksi

Monissa ohjelmointitehtävissä saattaa olla tarvetta käyttää satunnaislukuja. Ne voivat olla hyödyllisiä esimerkiksi pelien satunnaisessa generoinnissa tai simulaatioissa. C-kielessä on sisäänrakennettu toiminto, joka mahdollistaa satunnaislukujen generoinnin. Tässä blogikirjoituksessa esittelemme, miten sitä voidaan käyttää.

## Miten

Satunnaislukujen generoimiseen C-kielessä tarvitaan srand- ja rand-funktiot. Ensiksi tulee asettaa srand-funktiolle siemenluku, jota käytetään satunnaisten lukujen generointiin. Tämän jälkeen voidaan kutsua rand-funktiota, joka palauttaa satunnaisen luvun. Esimerkiksi:

```C
#include <stdio.h>
#include <stdlib.h>

int main() {
   int i, random;

   // Asetetaan siemenluku
   srand(1234);

   // Generoidaan 10 satunnaista lukua ja tulostetaan ne
   for(i = 0; i < 10; i++) {
      random = rand();
      printf("%d\n", random);
   }

   return 0;
}
```

Ohjelman ulostulo voi olla esimerkiksi seuraava:

```
22216
21414
884
17733
15217
8397
1877
416
10564
4046
```

Huomaa, että samalla siemenluvulla asetettaessa rand-funktio palauttaa saman sarjan satunnaisia lukuja.

## Syvempi sukellus

Satunnaislukujen generoiminen perustuu tietokoneen kellosta otettuihin aikamerkintöihin ja siemenlukuun. Siemenluvun muuttaminen vaikuttaa myös generoituihin satunnaisiin lukuihin. Siksi onkin tärkeää asettaa fungtion srand(parametri) parametri satunnaislukuna, esim. srand(time(0)), jolloin satunnaisluku vaihtuu jokaisessa ohjelman suorituksessa.

Toinen hyödyllinen funktio C-kielessä on rand() % n, joka palauttaa satunnaisluvun väliltä 0 ja n-1. Tämä on kätevä keino rajoittaa satunnaislukujen alue haluttuun väliin.

## Katso myös

- [C-kirjasto: stdlib.h](https://www.tutorialspoint.com/c_standard_library/stdlib_h.htm)
- [srand() funktio](https://www.tutorialspoint.com/c_standard_library/c_function_srand.htm)
- [rand() funktio](https://www.tutorialspoint.com/c_standard_library/c_function_rand.htm)