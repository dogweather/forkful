---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
html_title:           "Arduino: Merkkijonon muuntaminen pieniksi kirjaimiksi"
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?

Merkkijonon muuttaminen pienikirjaimiksi on operaatio, jossa kaikki merkkijonon iso kirjaimet muunnetaan pieniksi kirjaimiksi. Koodaajat tekevät tämän usein helpottaakseen vertailuja ja hakuja, koska kirjainkoko ei tällöin vaikuta tulokseen.

## Miten se tehdään:

Tässä on yksinkertainen esimerkki C-kielessä tehdystä merkkijonon muunnoksesta:

```C
#include <ctype.h>
#include <stdio.h>
#include <string.h>

void muunnaPieniksi(char* str) 
{
    for(int i = 0; str[i]; i++){
      str[i] = tolower((unsigned char) str[i]);
    }
}

int main(void) 
{
    char str[] = "Hello World!";
    muunnaPieniksi(str);
    printf("%s", str);
    return 0;
}
```

Konsolille tulostuu: "hello world!"

## Syvemmälle 

1. Historiallinen yhteys: Merkkijonon muuntamisen pieniksi kirjaimiksi voi jäljittää vanhoista ohjelmointikielistä, kuten FORTRAN ja COBOL. Tämä osoittaa, että merkkijonon käsittelyn perusasiat ovat pysyneet samana vuosikymmenien ajan.

2. Vaihtoehdot: tolower()-funktion lisäksi on olemassa valtava määrä kirjastoja ja työkaluja, jotka tekevät saman asian, kuten Boost, Poco ja Qt. Kuitenkin ne tuovat mukanaan myös suurempia riippuvuuksia, jos tarvitset vain merkkijonon pienet kirjaimet.

3. Toteutuksen yksityiskohdat: tolower()-functio hyväksyy merkin ja tarkistaa, kuuluuko se ASCII-isojen kirjaimien alueelle. Jos näin on, se muuntaa kirjaimen vastaavaksi pieneksi kirjaimeksi. Vaikka merkit voivat olla erilaisia eri kielissä ja niiden kirjoitusjärjestelmissä, tämä funktio käsittelee vain ASCII-merkkejä standardin mukaisesti.

## Katso myös:

1. ASCII: Kuinka ASCII toimii? Katso lisää [täältä](https://fi.wikipedia.org/wiki/ASCII).
2. C Library - <ctype.h>: Tutustu in-depth [täällä](https://en.cppreference.com/w/cpp/header/cctype). 
3. String Manipulation: Lisää merkkijonon käsittelyn oppaita [täältä](https://www.programiz.com/c-programming/c-strings).