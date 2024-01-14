---
title:                "C: Merkkijonon muuntaminen isolla alkukirjaimella"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Miksi

Miksi haluaisit muuttaa merkkijonon ensimmäisen kirjaimen isoiksi kirjaimiksi? Yleensä tämä tehdään, jotta merkkijono voidaan esittää oikeassa muodossa, kuten henkilön nimi tai otsikko. 

## Miten

Capitalization voi tuntua yksinkertaiselta tehtävältä, mutta on olemassa useita tapoja saavuttaa se C-ohjelmoinnissa. Tässä esimerkkejä koodista, joka muuttaa merkkijonon ensimmäisen kirjaimen isoksi kirjaimeksi:

```C
// 1. Käyttäen ctype.h-kirjastoa
#include <stdio.h>
#include <ctype.h>

void capitalize(char* word) {
    if (word[0] != '\0') {
        word[0] = toupper(word[0]);
    }
}

int main() {
    char name[] = "matti";
    capitalize(name);
    printf("Nimi isolla alkukirjaimella: %s\n", name);
    return 0;
}
// Tulostus: Nimi isolla alkukirjaimella: Matti
```

```C
// 2. Käyttäen string.h-kirjastoa
#include <stdio.h>
#include <string.h>

void capitalize(char* word) {
    if (word[0] != '\0') {
        word[0] = toupper(word[0]);
    }
}

int main() {
    char name[] = "matti";
    capitalize(name);
    printf("Nimi isolla alkukirjaimella: %s\n", name);
    return 0;
}
// Tulostus: Nimi isolla alkukirjaimella: Matti
```

```C
// 3. Käyttäen ominaisuuden vertailua
#include <stdio.h>

void capitalize(char* word) {
    if (word[0] >= 97 && word[0] <= 122) {
        word[0] -= 32;
    }
}

int main() {
    char name[] = "matti";
    capitalize(name);
    printf("Nimi isolla alkukirjaimella: %s\n", name);
    return 0;
}
// Tulostus: Nimi isolla alkukirjaimella: Matti
```

## Syventävä tieto

Kaikissa esimerkeissä käytettiin char-tyyppistä merkkijonoa, mutta samaa logiikkaa voidaan soveltaa myös muihin tietotyyppeihin, kuten int tai double. Lisäksi huomioi, että esimerkit toimivat vain yhden sanan mittaisilla merkkijonoilla. Jos haluat muuttaa isoksi kirjaimeksi toisen tai useamman sanan, niiden välissä olevat välilyönnit tulee ottaa huomioon. 

## Katso myös

- [C:n ctype.h-kirjasto](https://www.tutorialspoint.com/c_standard_library/ctype_h.htm)
- [C:n string.h-kirjasto](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [Char-tyypin dokumentaatio](https://www.cplusplus.com/reference/cstdlib/strtod/)
- [String-tyypin dokumentaatio](https://www.cplusplus.com/reference/cstring/strcat/)