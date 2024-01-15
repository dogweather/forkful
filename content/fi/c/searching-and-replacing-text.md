---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "C: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi
Miksi joku haluaa etsiä ja korvata tekstiä ohjelmoinnin yhteydessä? Yksi syy voi olla löytää ja vaihtaa tiettyjä tekstinpätkiä helposti ja nopeasti, mikä auttaa säästämään aikaa ja vähentämään virheiden mahdollisuutta.

## Miten
Kun haluat etsiä ja korvata tekstiä C-ohjelmassasi, voit käyttää sisäänrakennettua "str_replace" -funktiota, joka korvaa kaikki annetun merkkijonon esiintymät toisella merkkijonolla. Alla on yksinkertainen esimerkki koodista:

```C
#include <stdio.h>
#include <string.h>

int main() {
    char str[50] = "Tämä on esimerkkiteksti";
    
    // Etsitään ja korvataan "on" merkkijonossa "in" avulla
    str_replace(str, "on", "in");
    
    printf("%s", str);
    
    return 0;
}
```

Tässä tapauksessa koodi tulostaa "Tämä in esimerkkiteksti".

## Syvällinen sukellus
C-kielen sisäänrakennettujen funktioiden lisäksi voit myös käyttää "regex" -kirjastoa (regex.h), joka tarjoaa tehokkaampia ominaisuuksia tekstinpätkien etsimiseen ja korvaamiseen. Regex-kirjaston avulla voit käyttää säännöllisiä lausekkeita etsiessäsi ja korvatessasi tekstiä, mikä tarjoaa suuremman joustavuuden ja monipuolisuuden.

Jos haluat lisätietoja säännöllisistä lausekkeista ja niiden käyttämisestä C-ohjelmissa, suosittelemme tutustumaan alla oleviin linkkeihin:

* [C Manual - Regular Expressions](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html)
* [C Regex Library Documentation](https://www.regular-expressions.info/c.regex.html)

## Katso myös
* [C Documentation](https://en.cppreference.com/w/c)
* [C++ - Searching and Replacing Text](https://dev.to/shin8/c-searching-and-replacing-text-1bo9) (englanninkielinen artikkeli)