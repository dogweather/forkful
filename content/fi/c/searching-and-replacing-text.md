---
title:                "Tekstin etsiminen ja korvaaminen"
date:                  2024-01-20T17:57:37.364703-07:00
model:                 gpt-4-1106-preview
simple_title:         "Tekstin etsiminen ja korvaaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Mitä & Miksi?)
Hakeminen ja korvaaminen on tekstinpätkien löytämistä ja niiden muuttamista. Koodarit käyttävät tätä automatisoidakseen tylsiä tehtäviä ja välttääkseen inhimillisiä virheitä.

## How to: (Kuinka tehdä:)
```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *source, const char *search, const char *replace) {
    char buffer[1024];
    char *insert_point = &buffer[0];
    const char *tmp = source;
    size_t search_len = strlen(search);
    size_t replace_len = strlen(replace);

    while (1) {
        const char *p = strstr(tmp, search);

        if (p == NULL) {
            strcpy(insert_point, tmp);
            break;
        }

        memcpy(insert_point, tmp, p - tmp);
        insert_point += p - tmp;

        memcpy(insert_point, replace, replace_len);
        insert_point += replace_len;

        tmp = p + search_len;
    }

    strcpy(source, buffer);
}

int main() {
    char text[] = "Hyvää päivää, maailma!";
    const char *oldWord = "maailma";
    const char *newWord = "kaikki";

    printf("Alkuperäinen: %s\n", text);
    searchAndReplace(text, oldWord, newWord);
    printf("Korvattu: %s\n", text);

    return 0;
}
```

Sample Output:
```
Alkuperäinen: Hyvää päivää, maailma!
Korvattu: Hyvää päivää, kaikki!
```

## Deep Dive (Syväsukellus)
Historiallisesti, tekstinhakua ja -korvausta varten oli käytetty yksinkertaisia komentosarjoja tai editorien sisäänrakennettuja toimintoja. Modernissa ohjelmoinnissa on monia kirjastoja, jotka tarjoavat näitä toimintoja. `strstr` on C standardikirjaston funktio tekstinhakuun, ja `strcpy` sekä `memcpy` ovat tietojen kopiointiin. Tekstinkäsittely vaatii huolellisuutta puskurin ylivuotojen ja muiden muistiongelmien välttämiseksi.

Vaihtoehtoisesti voidaan käyttää regular expressions -kirjastoa (regex) monimutkaisempiin hakuihin. C-standardikirjastossa ei ole valmista regex-tukea, joten usein käytetään ulkoisia kirjastoja, kuten PCRE.

Hakemista ja korvaamista toteutettaessa on tärkeää huomioita suorituskyky, erityisesti suurilla tekstimäärillä. Muistinkäyttö, algoritmin tehokkuus ja monimutkaisuus vaikuttavat kaikki.

## See Also (Katso Myös)
- C Standard Library documentation: https://en.cppreference.com/w/c/string/byte
- PCRE - Perl Compatible Regular Expressions: https://www.pcre.org/
- Mastering Regular Expressions - Jeffrey E.F. Friedl: https://shop.oreilly.com/product/9780596528126.do
