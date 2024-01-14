---
title:                "C: Virheenjäljitystulostaminen"
programming_language: "C"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Joskus koodia kirjoittaessa voi ilmetä tilanteita, joissa tarvitaan tarkempaa tietoa siitä, miten koodi suorittaa kunkin tehtävänsä. Debug-tulostus on hyödyllinen työkalu, joka auttaa löytämään ja korjaamaan mahdollisia virheitä ohjelmassa.

## Miten tehdä

```C
#include <stdio.h>

int main() {
  int sum = 0;
  // Luodaan debug-tulostus, joka näyttää jokaisen luvun, joka lisätään summaan
  for (int i = 0; i < 10; i++) {
    sum += i;
    printf("Luku %d lisätty summaan\n", i);
  }
  // Tulostetaan summa
  printf("Summa: %d\n", sum);

  return 0;
}
```

Tässä esimerkissä käytetään C:n `printf`-funktiota tulostamaan haluttu viesti koodin suorituksen aikana. Debug-tulosteet voidaan lisätä eri kohdille koodia tarpeen mukaan, jotta nähdään tarkemmin, miten koodin suoritus etenee.

## Syvempi sukellus

Debug-tulostus on hyödyllinen ennen kaikkea ohjelman kehitysvaiheessa ja auttaa löytämään mahdollisia virheitä, kuten muuttujien arvojen muutoksia ja ohjausvirtoja. Kuitenkin, kun ohjelma on valmis, turhat debug-tulostukset tulisi poistaa, sillä ne voivat hidastaa ohjelman suoritusta.

## Katso myös

- [What is Debugging and how to get started](https://www.guru99.com/debugging-principles.html)
- [How to debug in C](https://www.cs.swarthmore.edu/~newhall/unixhelp/debuggingtips_C.html)
- [Debugging in C - Techniques](https://www.tutorialcup.com/debugging/c-techniques.htm)