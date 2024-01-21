---
title:                "Lese en tekstfil"
date:                  2024-01-20T17:53:57.867633-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lese en tekstfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Å lese en tekstfil betyr at programmet ditt henter data fra en fil på disken - typisk i form av tekst. Vi gjør dette fordi vi ofte trenger å behandle eller bruke data som er lagret utenfor vårt program, og filer er et vanlig format for sådan lagring.

## How to:
```C
#include <stdio.h>
#include <stdlib.h>

int main() {
    FILE *file;
    char ch;
    
    file = fopen("eksempel.txt", "r"); // Åpner filen for lesing
    if (file == NULL) {
        perror("Feil ved åpning av filen");
        return EXIT_FAILURE;
    }
    
    while((ch = fgetc(file)) != EOF) { // Leser en karakter om gangen
        putchar(ch); // Skriver ut karakteren
    }
    
    fclose(file); // Lukker filen
    return EXIT_SUCCESS;
}
```
Sample output for a file named "eksempel.txt" containing "Hei, verden!":
```
Hei, verden!
```

## Deep Dive
Lesing av tekstfiler i C har røtter tilbake til starten av språket på 70-tallet. `fopen`, `fread`, `fgets`, og `fgetc` er blant de klassiske funksjonene som har stått testen av tid. Alternativer til standard C fil-I/O inkluderer høyere nivå biblioteker som `stdio.h` for enklere bruk, eller direkte systemkall for mer kontroll og ytelse. Måten disse funksjonene fungerer på, gjennom en filbuffer og et filpekere, er essensiell for effektiviteten i å håndtere filer, spesielt store filer.

## See Also
- [C Standard Library - File Input/Output](https://en.cppreference.com/w/c/io)
- [GNU C Library - Input/Output on Streams](https://www.gnu.org/software/libc/manual/html_node/I_002fO-Overview.html)