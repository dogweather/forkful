---
title:                "Tekstin etsiminen ja vaihtaminen"
html_title:           "C: Tekstin etsiminen ja vaihtaminen"
simple_title:         "Tekstin etsiminen ja vaihtaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Hakeminen ja tekstikorvaaminen ovat yleisiä ohjelmointitehtäviä, joissa muokataan tekstiä ohjelmakoodissa. Tämä tehdään yleensä virheiden korjaamiseksi tai halutun toiminnallisuuden toteuttamiseksi.

## Kuinka:

Etsiessä ja korvatessa tekstiä C-koodissa, voit käyttää apuna esimerkiksi sisäänrakennettua funktiota `strstr`, joka etsii annetusta merkkijonosta toisen merkkijonon esiintymän ja palauttaa osoittimen kyseiseen kohtaan. Tämän avulla voit sitten korvata halutun tekstin uudella tekstillä käyttämällä esimerkiksi `strcpy`-funktiota.

```C
#include <stdio.h>
#include <string.h>

int main()
{
  char sana[20];
  char korvaava[20];
  char *osoitin;
  
  strcpy(sana, "Tämä on testi");
  strcpy(korvaava, "esimerkki");
  
  osoitin = strstr(sana, "testi");
  if (osoitin != NULL)
  {
    strcpy(osoitin, korvaava);
  }
  
  printf("%s", sana); // Tulostaa "Tämä on esimerkki"
  
  return 0;
}
```

## Syvempää tietoa:

Hakeminen ja tekstinkorvaaminen ovat olleet osa ohjelmointia jo pitkään, ja on olemassa monia muitakin tapoja suorittaa näitä tehtäviä. Esimerkiksi `sed`-työkalu Unix-ympäristöissä on suosittu tekstinkäsittelyyn tarkoitettu ohjelma, joka tarjoaa monipuolisia toimintoja tekstien etsimiseen ja korvaamiseen.

Etsimisprosessissa voidaan myös hyödyntää erilaisia algoritmeja, kuten Boyer-Moore-algoritmia, jotka pyrkivät tehostamaan hakutoimintoa erityisesti suurissa tekstimassoissa.

## Katso myös:

Voit lukea lisää hakemisesta ja korvaamisesta C-koodissa täältä: [https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm](https://www.tutorialspoint.com/c_standard_library/c_function_strstr.htm)