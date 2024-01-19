---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Säännölliset lausekkeet (engl. regular expressions) ovat merkkijonohakutyökaluja, joilla voi löytää ja vaihtaa tietyt kuviot tekstistä. Ne säästävät ohjelmoijien aikaa ja energiaa, koska niiden avulla voidaan tehdä monimutkaisia hakuja ja manipulointeja teksteissä.

## Kuinka:
```C
#include <regex.h>   
#include <stdio.h>
#define MAX_MATCHES 1 // Määritellään maksimaalisten ottelujen määrä 

void match_regex(char *to_search) {
    regex_t regex_compiled;
    regmatch_t group_array[MAX_MATCHES];

    if (regcomp(&regex_compiled, "[a-z]+", REG_EXTENDED)) {
        printf("Could not compile regular expression.\n");
        return;
    };
    
    if (regexec(&regex_compiled, to_search, MAX_MATCHES, group_array, 0) == 0)  {
        char source_copy[strlen(to_search) + 1];
        strcpy(source_copy, to_search);
        source_copy[group_array[0].rm_eo] = 0;
        printf("Matched: %s\n", source_copy + group_array[0].rm_so);
    } else {
        printf("No matches found.\n");
    }
    
    // Muistin vapautus
    regfree(&regex_compiled);
}

int main() {
    match_regex("lautaselle");
    match_regex("opeinohjelmoimaan");
    return 0;
}
```
Otosta:
```
Matched: lautaselle
Matched: opeinohjelmoimaan
```

## Syvällinen sukellus

Säännöllisiä lausekkeita ovat käyttäneet ohjelmoijat jo vuodesta 1956, kun ne esiteltiin automaattiteorian osana. Vaihtoehtoja on useita eri kieliä ja kirjastoja, kuten Perl, Python ja JavaScript. C:ssä säännöllisten lausekkeiden toteutus löytyy POSIX-kirjastosta. Säännölliset lausekkeet ovat tehokkaita, mutta niillä on haasteensa, kuten ylläpidettävyyden vaikeus ja heikko luettavuus. Naive-suodin voi olla parempi vaihtoehto yksinkertaisille kuvioille.

## Katso myös

* Ohjelmoinnin säännölliset lausekkeet (Wikipedia): https://fi.wikipedia.org/wiki/Säännöllinen_lauseke
* POSIX regular expressions (GNU) : https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html
* Mastering Regular Expressions (O'Reilly): https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/
* RegexOne: Learn Regular Expressions: https://regexone.com/