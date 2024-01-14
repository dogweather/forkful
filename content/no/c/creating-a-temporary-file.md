---
title:    "C: Lage en midlertidig fil"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å opprette midlertidige filer er en vanlig praksis i programmering, spesielt når man håndterer store datamengder. Disse filene brukes til å lagre midlertidig data under kjøretiden til et program, og er spesielt nyttige for å minimere mengden av informasjon som må lagres i hovedminnet.

## Hvordan

Det er flere måter å opprette midlertidige filer på i C-programmering. En vanlig metode er å bruke standardbibliotekfunksjonen `tmpfile()`, som oppretter en midlertidig fil og returnerer en peker til den.

```C
#include <stdio.h>
int main() {
    FILE *fp;
    fp = tmpfile();
    if (!fp) {
        printf("Kunne ikke opprette midlertidig fil.");
        return 1;
    }
    printf("Midlertidig fil opprettet.\n");
    fclose(fp);
    return 0;
}
```

```bash
$ gcc temp_file.c -o temp_file
$ ./temp_file
Midlertidig fil opprettet.
```

En annen måte er å bruke funksjonen `mkstemp()`, som oppretter en midlertidig fil og returnerer et filnavn.

```C
#include <stdio.h>
#include <stdlib.h>
int main() {
    char file_name[20];
    int fd;
    fd = mkstemp(file_name);
    if (fd == -1) {
        printf("Kunne ikke opprette midlertidig fil.");
        return 1;
    }
    printf("Midlertidig fil opprettet.\n");
    close(fd);
    remove(file_name);
    return 0;
}
```

```bash
$ gcc temp_file.c -o temp_file
$ ./temp_file
Midlertidig fil opprettet.
```

## Dypdykk

Det er viktig å sørge for at midlertidige filer blir slettet når de ikke lenger er nødvendige. Dette kan gjøres ved å bruke funksjonene `fclose()` eller `close()` etter behov, og til slutt slette filen ved hjelp av `remove()`.

Det er også viktig å være klar over at midlertidige filer ikke er immune mot feil og sikkerhetsangrep. Det er derfor viktig å sjekke for feil og validere inputen før du skriver til en midlertidig fil.

## Se også

- [How to Manage Temporary Files in C](https://www.linuxjournal.com/content/how-manage-temporary-files-c)
- [Create a Temporary File in C](https://www.programmingalgorithms.com/tutorial/create-a-temporary-file-in-c)
- [Temporary Files in C](https://www.geeksforgeeks.org/temporary-files-c/)