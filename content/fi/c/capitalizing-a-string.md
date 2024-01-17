---
title:                "Muutetaan merkkijonoa isolla alkukirjaimella"
html_title:           "C: Muutetaan merkkijonoa isolla alkukirjaimella"
simple_title:         "Muutetaan merkkijonoa isolla alkukirjaimella"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mitä ja miksi?

Tervehdys, ohjelmoijat! Olet varmasti joskus nähnyt sanaan "CAPS" kirjoitettuna isoilla kirjaimilla. Tämä tarkoittaa tekstin isossa kirjoituksessa ja se on yleinen tapa merkitä tekstiä tietokoneessa. Kun ohjelmoijat "kapitalisoivat" merkkijonoja, he tekevät niistä isoja kirjaimia. Tämä on hyödyllistä, kun yrittävät löytää tietyn sanan tai merkkijonon isolla tai pienellä kirjoituksella.

## Miten?

Tässä on lyhyt esimerkki siitä, miten voit kapitalisoida merkkijonon C-kielellä:

```C
#include <stdio.h>

// Funktio, joka muuntaa merkkijonon isojen kirjainten muotoon
void capitalize(char *str) {
   int i;

   // Käydään läpi jokainen merkki merkkijonosta
   for (i = 0; str[i] != '\0'; i++) {
      // Jos merkki on pieni kirjain, muutetaan se isoksi kirjaimeksi
      if (str[i] >= 'a' && str[i] <= 'z') {
         str[i] = str[i] - 32;
      }
   }
}

int main() {
   char str[50] = "ohjelmointi on hauskaa";

   printf("Alkuperäinen merkkijono on: %s\n", str);

   // Kutsutaan capitalize-funktiota ja tulostetaan muokattu merkkijono
   capitalize(str);
   printf("Muutettu merkkijono on: %s\n", str);

   return 0;
}
```

Tämä koodi tulostaa:

```
Alkuperäinen merkkijono on: ohjelmointi on hauskaa
Muutettu merkkijono on: OHJELMOINTI ON HAUSKAA
```

## Syvempi sukellus

Tietokoneissa käytetään yleensä ASCII-koodausta merkkien tallentamiseen ja käsittelyyn. Tämä tarkoittaa, että jokaisella merkillä on oma numeronsa ja pienet ja isot kirjaimet ovat eri numeroiden takana. Pienestä isoksi muuntamisen yksinkertainen logiikka perustuu ASCII-koodien numeroiden lisäämiseen tai vähentämiseen. Toiset kielet, kuten Java, tarjoavat valmiin capitalized-funktion, mutta C-kielessä sitä täytyy kirjoittaa itse.

## Katso myös

[C-kirjasto](https://www.cprogramming.com/tutorial/c/lesson14.html)
[ASCII-koodit](https://www.cs.cmu.edu/~pattis/15-1XX/common/handouts/ascii.html)