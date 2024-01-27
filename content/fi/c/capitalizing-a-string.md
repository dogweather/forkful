---
title:                "Merkkijonon muuttaminen isoiksi kirjaimiksi"
date:                  2024-01-19
html_title:           "Arduino: Merkkijonon muuttaminen isoiksi kirjaimiksi"
simple_title:         "Merkkijonon muuttaminen isoiksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Kun puhutaan merkkijonojen suurentamisesta, tarkoitetaan prosessia, jossa jokainen merkkijonon kirjain muunnetaan isoksi kirjaimeksi. Se tehdään usein käyttöliittymissä tai datan esittämisessä, jotta teksti erottuisi tai olisi yhdenmukaista.

## How to: (Kuinka tehdään:)
```C
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "tervetuloa ohjelmointiin!";
    capitalizeString(myString);
    printf("Capitalized string: %s\n", myString);
    return 0;
}
```

### Sample output:
```
Capitalized string: TERVETULOA OHJELMOINTIIN!
```

## Deep Dive (Sukellus syvyyksiin)
Historian valossa merkkijonojen suurentamisella on juurensa kirjoituskoneissa ja varhaisissa tietojenkäsittelyjärjestelmissä, joissa oli vain isoja kirjaimia. Nykyisissä ohjelmointikielessä se on enemmän kysymys muotoilusta ja standardisaatiosta. Vaihtoehtoisia tapoja toteuttaa suurentaminen ovat kirjastofunktiot, kuten `strupper` eri kielissä tai oman funktion kirjoittaminen, kuten yllä. Tärkeintä on tiedostaa, että funktio `toupper` odottaa merkkejä, joiden tyyppi on `unsigned char`, jotta laajennetun ASCII-taulukon merkit käsitellään oikein.

## See Also (Lisää luettavaa)
- C Standard Library documentation for `ctype.h`: https://en.cppreference.com/w/c/header/ctype
- ASCII table reference: https://www.asciitable.com/
- Stack Overflow discussions about string manipulation in C: https://stackoverflow.com/questions/tagged/c+strings
