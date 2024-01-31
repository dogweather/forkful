---
title:                "Bruke et interaktivt skall (REPL)"
date:                  2024-01-26T04:11:59.342975-07:00
model:                 gpt-4-0125-preview
simple_title:         "Bruke et interaktivt skall (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En interaktiv shell, eller Les-Evaluer-Skriv Løkke (REPL), er et verktøy som gir et sanntids kodingsmiljø for å teste kodesnutter umiddelbart. Programmerere bruker det for rask tilbakemelding under utvikling, læring, og feilsøking.

## Hvordan:
C kommer ikke med en innebygd REPL, men du kan bruke tredjepartsverktøy. Her er et innblikk ved hjelp av Cling, en C++ tolk som også kan håndtere C-kode:

```C
#include <stdio.h>

int main() {
    printf("Hei, REPL-verden!\n");
    return 0;
}
```

Utskrift i Cling REPL:
```
[cling]$ .x yourscript.c
Hei, REPL-verden!
```

Cling utfører skriptet og skriver ut resultatet umiddelbart.

## Dypdykk
REPLs er standard i dynamiske språk som Python eller Ruby, men for kompilerte språk som C, er de mindre vanlige. Historisk sett har kompiler-kjør-feilsøk syklusen ikke lent seg til interaktiv utforskning. Verktøy som Cling og online C-kompilatorer tilbyr REPL-lignende opplevelser ved å pakke inn C-koden din i et C++-miljø.

Alternativer til Cling inkluderer C-tolkere som CINT og Ch. Disse verktøyene tillater rask iterasjon, men kan ikke være egnet for alle utviklingsscenarier på grunn av prestasjonsbegrensninger og støtte for komplekse funksjoner.

Implementering av en REPL i et kompilert språk innebærer kompilering og utførelse av kodesnutter på fly, noe som er ikke-trivielt og kan ha begrensninger sammenlignet med de fullstendige språkevne.

## Se også
- Cling: https://github.com/root-project/cling
- Online C Compiler og REPL: https://repl.it/languages/c
- CINT: http://root.cern.ch/drupal/content/cint
- Ch Tolk: http://www.softintegration.com/products/chstandard/
