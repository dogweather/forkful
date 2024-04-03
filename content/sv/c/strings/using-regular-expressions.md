---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:57.705225-07:00
description: "Hur man g\xF6r: F\xF6r att anv\xE4nda regulj\xE4ra uttryck i C kommer\
  \ du fr\xE4mst att arbeta med POSIX regex-biblioteket (`<regex.h>`). Det h\xE4r\
  \ exemplet demonstrerar\u2026"
lastmod: '2024-03-13T22:44:38.371405-06:00'
model: gpt-4-0125-preview
summary: "F\xF6r att anv\xE4nda regulj\xE4ra uttryck i C kommer du fr\xE4mst att arbeta\
  \ med POSIX regex-biblioteket (`<regex.h>`)."
title: "Anv\xE4nda regulj\xE4ra uttryck"
weight: 11
---

## Hur man gör:
För att använda reguljära uttryck i C kommer du främst att arbeta med POSIX regex-biblioteket (`<regex.h>`). Det här exemplet demonstrerar grundläggande mönstermatchning:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Mönster för att matcha strängar som börjar med 'a' följt av alfanumeriska tecken
    char *test_string = "apple123";

    // Kompilera det reguljära uttrycket
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Kunde inte kompilera regex\n");
        exit(1);
    }

    // Exekvera det reguljära uttrycket
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Matchning hittades\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Ingen matchning hittades\n");
    } else {
        printf("Regex matchning misslyckades\n");
        exit(1);
    }

    // Frigör allokerat minne som används av regex
    regfree(&regex);

    return 0;
}
```

Exempelutdata för en matchande sträng ("apple123"):
```
Matchning hittades
```
Och för en icke-matchande sträng ("banana"):
```
Ingen matchning hittades
```

## Fördjupning:
Reguljära uttryck i C, som en del av POSIX-standarden, erbjuder ett robust sätt att utföra strängmatchning och manipulation. Dock betraktas API:et för POSIX regex-biblioteket i C som mer omständligt än de som finns i språk som är designade med inbyggda strängmanipuleringsfunktioner som Python eller Perl. Syntaxen för mönster är liknande över språk, men C kräver manuell hantering av minne och mer boilerplate-kod för att förbereda, exekvera och städa upp efter att ha använt regex-mönster.

Trots dessa utmaningar är det belönande att lära sig använda regex i C eftersom det fördjupar förståelsen av programmering på lägre nivå. Dessutom öppnar det upp möjligheter för C-programmering inom områden som textbehandling och dataextraktion där regex är oumbärligt. För mer komplexa mönster eller regex-operationer kan alternativ som PCRE (Perl Compatible Regular Expressions) biblioteket erbjuda ett mer funktionellt och något enklare gränssnitt, även om det kräver att en extern bibliotek integreras i ditt C-projekt.
