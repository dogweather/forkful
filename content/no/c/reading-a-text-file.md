---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er en prosess hvor programmeret henter innholdet i en fil for å bruke det til noe nyttig. Programmerere gjør dette for å behandle data, konfigurere systeminnstillinger eller importere data fra filer lagret av andre programmer.

## Hvordan gjør man det:

Her er en enkel måte å lese en tekstfil i C:

```C
#include <stdio.h>

int main () {
  FILE * file;
  char line [128]; 
  
  file = fopen ("example.txt" , "r");
  if (file == NULL) perror ("Error opening file");
  
  while ( fgets (line, sizeof line, file) != NULL ) 
    fputs (line, stdout);
  
  fclose (file);
  return 0;
}
```

Koden over åpner `example.txt` for lesing og skriver ut hver linje til standard output. Hvis `example.txt` inneholder:

```C
Hallo Verden!
Jeg er en tekstfil.
```

Vil output være:

```C
Hallo Verden!
Jeg er en tekstfil.
```

## Dypdykk:

C, som ble laget i 1972, er blitt en standard for filhåndtering takket være effektiviteten og fleksibiliteten til funksjonene.

Alternativ til `fgets()` kan være funksjonene `fscanf()` og `fread() `. `fscanf() ` leser filen basert på spesifiserte formater, mens `fread() ` leser filen byte-for-byte. Valgene avhenger av bruksområdet.

Når det gjelder implementering av fillesing, er det viktig å huske på at filhåndtering kan føre til minnelekkager hvis det ikke håndteres skikkelig. Alltid lukk filene etter bruk med `fclose()`.

## Se Også:

Her er noen nyttige lenker for mer informasjon: