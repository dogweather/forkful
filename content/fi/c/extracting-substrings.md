---
title:                "Merkkijonojen osien poimiminen"
date:                  2024-01-20T17:45:03.554099-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonojen osien poimiminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (Mikä & Miksi?)
Substringit ovat jonoja, jotka on otettu isommista merkkijonoista. Ohjelmoijat käyttävät niitä, koska usein tarvitaan vain tietyt osat datasta - esimerkiksi käyttäjänimen etsiminen sähköpostiosoitteesta.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[] = "Tervetuloa Suomeen!";
    char substr[10];

    strncpy(substr, str + 11, 7); // Kopioidaan 7 merkkiä alkaen indeksistä 11.
    substr[7] = '\0'; // Lisätään nollamerkki loppuun.

    printf("Substring: '%s'\n", substr);
    return 0;
}
```

Sample output:
```
Substring: 'Suomeen'
```

## Deep Dive (Sukellus syvyyksiin):
Substringien käsittely C:ssä ei tue äärettömän pitkiä merkkijonoja, joten pituuden hallinta on tärkeää. Historiallisesti, C:n standardikirjastoa on arvosteltu sen turvallisuuden puutteesta – strncpy-funktion kanssa pitää muistaa lisätä nollamerkki. Vaihtoehtoisesti voitaisiin käyttää memmove-funktiota, joka toimii bittitasolla, mutta sekin vaatii huolellisuutta.

Modernina vaihtoehtona ovat kielet kuten Python tai JavaScript, jotka tarjoavat substringien käsittelyyn selkeämmät työkalut. C:ssä kuitenkin tämä yksinkertainen operaatio vaatii tarkkuutta erityisesti muistinhallinnassa – ole tarkkana puskurin ylivuotojen kanssa.

## See Also (Katso myös):
- C Standard Library, `string.h`: https://en.cppreference.com/w/c/string/byte
- Secure coding in C: https://www.owasp.org/index.php/C-Based_Toolchain_Hardening
- C manual: http://devdocs.io/c/
