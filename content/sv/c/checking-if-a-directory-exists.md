---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-19
html_title:           "Arduino: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"

category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (Vad & Varför?)
Att kontrollera om en katalog finns är processen att säkerställa att en specifik mapp finns på filsystemet. Programmerare gör detta för att undvika fel när de till exempel ska läsa från eller skriva till filer i katalogen.

## How to (Hur man gör)
Använd `stat()` från `sys/stat.h` för att kontrollera kataloger.

```C
#include <sys/stat.h>
#include <stdio.h>

int main() {
    struct stat statbuf;
    char *dirPath = "/path/to/directory";

    // Kontrollera om katalogen finns
    if (stat(dirPath, &statbuf) == 0 && S_ISDIR(statbuf.st_mode)) {
        printf("Katalogen finns.\n");
    } else {
        printf("Katalogen finns inte.\n");
    }

    return 0;
}
```

Sample output för en existerande katalog:
```
Katalogen finns.
```

Sample output för en icke-existerande katalog:
```
Katalogen finns inte.
```

## Deep Dive (Djupdykning)
Funktionen `stat()` har en lång historia i Unix-baserade system, där den infördes för att hämta filstatus. I C gör `stat()` samma sak: den hämtar filattribut för den angivna sökvägen. 

Alternativa metoder:
- `opendir()` från `dirent.h` kan också användas men öppnar katalogen istället för att endast kontrollera dess existens.
- `access()` med `F_OK` kan kontrollera tillgängligheten av filen/katalogen, men den ger inte detaljerad information om det är en fil eller katalog.
  
Implementation:
- `stat()` fyller `stat`-strukturen med information om filen/katalogen. `st_mode` innehåller filtypen och rättigheterna.
- Makrot `S_ISDIR()` används för att kontrollera om `st_mode` indikerar en katalog.
- En nolla returneras vid framgång och `-1` vid fel. Använd `errno` för att få mer specifik felinformation.

## See Also (Se även)
- POSIX `stat` manpage: http://man7.org/linux/man-pages/man2/stat.2.html
- GNU C Library: https://www.gnu.org/software/libc/manual/
- Stack Overflow – Common C file operations: https://stackoverflow.com/questions/tagged/c+file
- `opendir()` dokumentation: https://linux.die.net/man/3/opendir
- `access()` system call: https://www.man7.org/linux/man-pages/man2/access.2.html
