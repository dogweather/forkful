---
title:                "Søking og erstatting av tekst"
date:                  2024-01-20T17:57:20.420126-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Tekstsøk og -erstattning lar oss finne spesifikke ord eller fraser og bytte dem ut med noe annet. Hvorfor? Det sparer tid, automatiserer kjedelige oppgaver og reduserer menneskelige feil i kode eller dokumenter.

## How to:
Nedenfor finner du en enkel C-programkode som bruker `strstr` og `strcpy` for å søke og erstatte tekst.

```C
#include <stdio.h>
#include <string.h>

void searchAndReplace(char *source, const char *search, const char *replace) {
    char buffer[1024]; 
    char *pos;
    int index = 0;

    while ((pos = strstr(source, search)) != NULL) {
        strcpy(buffer, source);
        index = pos - source;
        source[index] = '\0';
        strcat(source, replace);
        strcat(source, buffer + index + strlen(search));
    }
}

int main() {
    char text[] = "Eple er godt, og jeg liker eple!";
    const char *search = "eple";
    const char *replace = "banan";

    searchAndReplace(text, search, replace);
    printf("Erstattet tekst: %s\n", text);
    return 0;
}
```

Forventet utskrift:

```
Erstattet tekst: Banan er godt, og jeg liker banan!
```

## Deep Dive
Søk og erstatning er en gammel idé. I tidlige dager av databearbeidning, var det en manuell jobb. Med fremveksten av tekstbehandlere på 60- og 70-tallet, ble det enklere.

`strstr` søker etter en understreng. `strcpy` og `strcat` håndterer kopiering og sammenslåing. Effektiviteten av disse funksjonene avhenger av implementasjonen og lengden på tekstene involvert.

Alternativer til C sin standardbibliotek inkluderer regulære uttrykk via biblioteker som PCRE (Perl Compatible Regular Expressions), og moderne språkfunksjoner som tilbyr mer kraftfulle tekstbehandling verktøy out-of-the-box.

I C er detaljert håndtering nødvendig fordi programmereren må passe på bufferstørrelser og minnehåndtering. Det er en del av språkets lave nivå natur som gir kraften og fleksibiliteten mange C-programmerere verdsetter.

## See Also
- C Standard Library documentation: https://en.cppreference.com/w/c/string
- PCRE Project: https://www.pcre.org/
- Regular Expressions info: https://www.regular-expressions.info/
