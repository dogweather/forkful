---
title:    "C: Muuntamassa merkkijonoa pieniksi kirjaimiksi"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kukaan haluaisi muuttaa merkkijonon pienaakkosiksi? Vaikka se saattaa kuulostaa yksinkertaiselta tehtävältä, on tämä muunnos tärkeä osa ohjelmointia monessa tilanteessa. Esimerkiksi silloin kun vertaillaan merkkijonoja, on tärkeää että molemmat merkkijonot ovat samassa muodossa.

## Kuinka tehdä

Merkkijonon muuttaminen pienaakkosiksi tapahtuu käyttäen C:n standardikirjastofunktiota `tolower`. Tämä funktio muuntaa yhden merkin kerrallaan ja sitä voi käyttää merkkijonon läpikäymiseen for-silmukan avulla. Alla esimerkki koodista ja sen tuottamasta tulosteesta:

```C
#include <stdio.h>
#include <ctype.h>

int main() {
  char string[] = "Tämä On MERRKIJONO PiENAkkOsina"; 
  int i; 

  for (i = 0; string[i] != '\0'; i++) {
    string[i] = tolower(string[i]);
  }

  printf("%s\n", string); //tulostaa: "tämä on merkkijono pienaakkosina"
  return 0;
}
```

## Syväsukellus

`tolower`-funktio käyttää ASCII-taulukkoa muuntaakseen merkkinä olevan kirjaimen vastaavaan pienaakkoseen. Tämä tarkoittaa, että esimerkiksi tästä merkistä `K` tulee `k` ja merkistä `Ö` tulee `ö`. On hyvä muistaa, että tämä muunnos ei toimi muiden kirjaimistojen kanssa, kuten esimerkiksi skandinaavisissa kieletissä käytetyllä UTF-8-koodistikolla.

Tässä kohtaa voi myös mainita toisen hyödyllisen funktioon `toupper`, joka muuntaa merkkijonon isoaakkosiksi. Näiden kahden funktion yhdistelmä mahdollistaa merkkijonon muuntamisen eri muotoihin tarpeen mukaan.

## Katso myös

- [C Standard Library: tolower](https://www.cplusplus.com/reference/cctype/tolower/)
- [C Standard Library: toupper](https://www.cplusplus.com/reference/cctype/toupper/)