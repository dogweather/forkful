---
title:                "Een string met hoofdletters maken"
date:                  2024-01-28T21:55:30.773537-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string met hoofdletters maken"

category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/c/capitalizing-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het capitaliseren van een string betekent het omzetten van alle kleine letters naar hoofdletters. Programmeurs kapitaliseren vaak strings voor consistentie, weergaveformattering, of als onderdeel van datanormalisatieprocessen.

## Hoe te:
De C-taal heeft geen ingebouwde functie om strings te capitaliseren. Je zult doorgaans door elk karakter lopen, terwijl je onderweg kapitaliseert:

```c
#include <stdio.h>
#include <ctype.h>

void capitalizeString(char *str) {
    while (*str) {
        *str = toupper((unsigned char) *str);
        str++;
    }
}

int main() {
    char myString[] = "hello world!";
    capitalizeString(myString);
    printf("%s\n", myString);  // Uitvoer: HELLO WORLD!
    return 0;
}
```

## Diepere Duik
In de begindagen van computing waren bewerkingen op strings basisch en handmatig. C, ontwikkeld in de vroege jaren 70, weerspiegelt dit met eenvoudige stringmanipulatiefuncties in zijn standaardbibliotheek. De functie `toupper` is ontworpen om een enkel karakter naar hoofdletters te converteren. Het maakt deel uit van `<ctype.h>`, een header die functies bevat om karakters te testen en te mappen.

Er zijn alternatieven voor het doorlopen van een string om deze te capitaliseren. Bibliotheken zoals `libCStringUtils` bieden meer complexe stringbewerkingen, waaronder kapitalisatie. Sommige ontwikkelaars schrijven ook hun eigen functies met kenmerken zoals lokale gevoeligheid.

Intern hebben ASCII-karakters numerieke equivalenten, die 32 verschillen tussen hoofdletters en kleine letters. De `toupper`-functie gebruikt dit verschil om karakters te converteren. Echter, het direct vertrouwen op ASCII-waarden wordt niet aanbevolen vanwege leesbaarheids- en lokalisatieproblemen.

## Zie Ook
- Documentatie van de C Standaardbibliotheek: https://en.cppreference.com/w/c/header
- ASCII Tabel en Beschrijving: http://www.asciitable.com/
- GNU Libc handboek: https://www.gnu.org/software/libc/manual/html_node/String-and-Array-Utilities.html
