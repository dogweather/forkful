---
title:                "C: Kontroll av existerande mapp"
programming_language: "C"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Inom programmering är det viktigt att ha kontroll över vilka filer och mappar som befinner sig i ens arbetsmiljö. Därför är det värdefullt att lära sig hur man kan kontrollera om en viss mapp existerar eller inte. Så här kan du göra det i C-programmering!

## Hur du gör

```C
#include <stdio.h>
#include <unistd.h>

int main() {
  if (access("mapp1", F_OK ) != -1 ) {
    printf("Mappen existerar\n");
  } else {
    printf("Mappen existerar inte\n");
  }
  return 0;
}
```
 Det första du behöver göra är att inkludera header-filerna "stdio.h" och "unistd.h". Sedan använder vi funktionen "access()" som tar in två parametrar - sökvägen till mappen och "F_OK" som är en flagga för att kontrollera om en fil eller mapp existerar. Om mappen existerar kommer "access()" att returnera 0, annars kommer den att returnera -1. Vi använder en "if-else" sats för att skriva ut lämpligt meddelande beroende på resultatet.

## Utforskning

Det finns olika sätt att kontrollera om en mapp existerar i C-programmering. En annan metod är att använda funktionen "opendir()" och sedan kontrollera om den returnerade pekaren är null eller inte. Men detta kräver att du inkluderar header-filen "dirent.h". En annan intressant sak att notera är att "access()" också kan användas för att kontrollera om en fil existerar genom att ersätta "F_OK" med "R_OK", "W_OK" eller "X_OK" beroende på om du vill kontrollera för läs-, skriv- eller exekveringsrättigheter.

## Se även

- Dokumentation för "access()" funktionen: https://linux.die.net/man/2/access
- Mer information om att kontrollera fil- och mapp-behörigheter: https://www.cs.cmu.edu/afs/cs/academic/class/15213-f00/www/class27/final-paper-io.html