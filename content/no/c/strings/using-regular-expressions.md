---
aliases:
- /no/c/using-regular-expressions/
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:10:50.566384-07:00
description: "Regul\xE6re uttrykk (regex) gir en m\xE5te \xE5 s\xF8ke, matche og manipulere\
  \ strenger ved hjelp av definerte m\xF8nstre. Programmerere bruker dem omfattende\
  \ for\u2026"
lastmod: 2024-02-18 23:08:54.383554
model: gpt-4-0125-preview
summary: "Regul\xE6re uttrykk (regex) gir en m\xE5te \xE5 s\xF8ke, matche og manipulere\
  \ strenger ved hjelp av definerte m\xF8nstre. Programmerere bruker dem omfattende\
  \ for\u2026"
title: "Bruke regul\xE6re uttrykk"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Regulære uttrykk (regex) gir en måte å søke, matche og manipulere strenger ved hjelp av definerte mønstre. Programmerere bruker dem omfattende for oppgaver som å validere inndata, analysere tekstdata og finne mønstre innen store tekstfiler, noe som gjør dem til et kraftig verktøy i ethvert språk, inkludert C.

## Hvordan:

For å bruke regulære uttrykk i C, vil du primært jobbe med POSIX regex-biblioteket (`<regex.h>`). Dette eksemplet demonstrerer grunnleggende mønstersøking:

```c
#include <stdio.h>
#include <stdlib.h>
#include <regex.h>

int main(){
    regex_t regex;
    int return_value;
    char *pattern = "^a[[:alnum:]]"; // Mønster for å matche strenger som starter med 'a' etterfulgt av alfanumeriske tegn
    char *test_string = "apple123";

    // Kompiler regulært uttrykk
    return_value = regcomp(&regex, pattern, REG_EXTENDED);
    if (return_value) {
        printf("Kunne ikke kompilere regex\n");
        exit(1);
    }

    // Utfør det regulære uttrykket
    return_value = regexec(&regex, test_string, 0, NULL, 0);
    if (!return_value) {
        printf("Treff funnet\n");
    } else if (return_value == REG_NOMATCH) {
        printf("Ingen treff funnet\n");
    } else {
        printf("Regex-treff mislyktes\n");
        exit(1);
    }

    // Frigjør allokert minne brukt av regex
    regfree(&regex);

    return 0;
}
```

Eksempelutdata for en matchende streng ("apple123"):
```
Treff funnet
```
Og for en ikke-matchende streng ("banana"):
```
Ingen treff funnet
```

## Dypdykk:

Regulære uttrykk i C, som en del av POSIX-standarden, tilbyr en robust måte å utføre strengmatchende og -manipulering på. Imidlertid anses POSIX regex-bibliotekets API i C for å være mer tungvint enn de som finnes i språk designet med førsteklasses strengmanipulasjonsegenskaper som Python eller Perl. Syntaksen for mønstre er lik på tvers av språk, men C krever manuell hukommelsesstyring og mer kode som må skrives for å forberede, utføre og rydde opp etter bruk av regex-mønstre.

Til tross for disse utfordringene er det givende å lære å bruke regex i C fordi det fordype forståelsen av lavnivå programmeringskonsepter. I tillegg åpner det opp muligheter for C-programmering innen områder som tekstanalyse og dataekstraksjon der regex er uunnværlig. For mer komplekse mønstre eller regex-operasjoner kan alternativer som PCRE (Perl Compatible Regular Expressions)-biblioteket tilby et mer funksjonsrikt og noe enklere grensesnitt, selv om det krever integrering av et eksternt bibliotek i C-prosjektet ditt.
