---
title:                "Skriving av en tekstfil"
date:                  2024-01-19
html_title:           "Arduino: Skriving av en tekstfil"
simple_title:         "Skriving av en tekstfil"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Skriving til en tekstfil betyr å lagre data som tekst på disk. Dette gjøres så data kan lagres permanent og gjenbrukes av programmer eller mennesker.

## Hvordan gjør vi det:
```C
#include <stdio.h>

int main() {
    FILE *file = fopen("eksempel.txt", "w");
    if (file == NULL) {
        return -1;
    }
    fprintf(file, "Hei, verden!\n");
    fclose(file);
    return 0;
}
```
Output: En fil kalt `eksempel.txt` inneholder teksten `Hei, verden!`.

## Dypdykk
I gamle dager skrev man til tekstfiler fordi det var hovedmetoden for å bevare data. Alternativer nå inkluderer databaser og skytjenester. Implementeringsdetaljene innebærer funksjoner som `fopen()`, som åpner en fil; `fprintf()`, som skriver til en fil; og `fclose()`, som lukker filen.

## Se også
- [Official C Documentation](https://en.cppreference.com/w/c)
- [Learn-C.org](https://www.learn-c.org/)
- [File I/O in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
