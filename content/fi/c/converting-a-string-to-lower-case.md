---
title:                "Merkkijonon muuntaminen pieniksi kirjaimiksi"
date:                  2024-01-20T17:37:45.641922-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon muuntaminen pieniksi kirjaimiksi"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? / Mitä & Miksi?
Muuttaa isoja kirjaimia pieniksi merkkijonossa. Miksi? Tehdään vertailuja, tiedon käsittelyä ja käyttäjäsyötteiden normalisointia yksinkertaisemmaksi.

## How to / Miten
```c
#include <stdio.h>
#include <ctype.h>

void to_lowercase(char *str) {
    while (*str) {
        *str = tolower((unsigned char) *str);
        str++;
    }
}

int main() {
    char text[] = "Moi, MITEN menee?";
    to_lowercase(text);
    printf("%s\n", text);  // Tulostuu: "moi, miten menee?"
    return 0;
}
```

## Deep Dive / Syväsukellus
Ennen, C-kielen standardikirjastossa ei ollut funktiota pienentämään kirjainkokoa. Käytettiin silmukoita, muunneltiin ASCII-arvoja. Vaihtoehtoina `tolower`-funktion sijasta voisi käyttää ulkopuolista kirjastoa, kuten `boost`-kirjastoja C++:ssa. `tolower` sisällytti ISO/IEC 9899:1990 standardiin, osana `<ctype.h>`. Tärkeää: käytä `unsigned char` tyyppiä `tolower`-funktion kanssa, jotta negatiiviset arvot eivät aiheuta määrittelemätöntä käytöstä.

## See Also / Katso Myös
- C Standard Library documentation: https://en.cppreference.com/w/c/string/byte/tolower
- ASCII-taulukko ja selityksiä: https://www.asciitable.com/
- Boost C++ Libraries: https://www.boost.org/
