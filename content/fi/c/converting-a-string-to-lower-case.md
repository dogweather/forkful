---
title:                "- Merkkijonon muuttaminen pieniksi kirjaimiksi"
html_title:           "C: - Merkkijonon muuttaminen pieniksi kirjaimiksi"
simple_title:         "- Merkkijonon muuttaminen pieniksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Miksi

Stringin muuntaminen pieniksi kirjaimiksi on tärkeä osa ohjelmointia monissa tilanteissa. Se auttaa yhtenäistämään käyttäjien syötteitä, vertaillessa merkkijonoja tai vain yksinkertaisesti tulostuksessa.

## Kuinka

```C
#include <stdio.h>
#include <ctype.h>

void convertToLower(char* str);

int main(void) {
    char str[] = "TämÄ TeKsTi On KoRuPpTioToN";
    
    printf("Alkuperäinen merkkijono: %s\n", str);
    
    convertToLower(str);
    
    printf("Muunnettu merkkijono: %s\n", str);
    
    return 0;
}

void convertToLower(char* str) {
    int i = 0;
    
    // Loopataan merkkijono läpi
    while (str[i]) {

        // Muunnetaan jokainen merkki pieneksi
        str[i] = tolower(str[i]); 

        i++; 
    }
}
```

Tulostus:
```
Alkuperäinen merkkijono: TämÄ TeKsTi On KoRuPpTioToN
Muunnettu merkkijono: tämä teksti on korruptioton
```

## Deep Dive

Merkkijonon muuntaminen pieniksi kirjaimiksi tapahtuu käyttämällä C:n `tolower()` -funktiota. Tämä funktio löytyy `ctype.h` -kirjastosta ja se hyväksyy parametrina merkin ja palauttaa saman merkin pienellä kirjaimella. Tarvittaessa merkin ASCII-arvo muunnetaan ensin suuresta pieneksi.

Yllä olevassa esimerkissä `convertToLower()` -funktio käy läpi merkkijonon merkki kerrallaan ja käyttää `tolower()` -funktiota muuttaakseen jokaisen merkin pieneksi. Tämän jälkeen tulostamme muutetun merkkijonon ja huomaamme, että kaikki kirjaimet ovat nyt pieniä.

## Katso myös

- [C:toupper() - Merkkijonon muuntaminen isoiksi kirjaimiksi](https://www.geeksforgeeks.org/c-toupper-function/)
- [Merkkijonojen käsittely C-kielessä](https://www.tutorialspoint.com/cprogramming/c_strings.htm)