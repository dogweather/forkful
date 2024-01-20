---
title:                "Kontrollera om en katalog finns"
html_title:           "C: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att kolla om en katalog finns är att verifiera att en specifik mapp existerar i ett filsystem. Programmerare gör detta för att undvika felmeddelanden och undantag vid försök att interagera med en icke-existerande katalog.

## Så här gör du:
Här är en kodsnutt som visar hur du kan kontrollera om en katalog existerar med hjälp av `stat` funktionen i C.

```C
#include <sys/stat.h>
#include <stdio.h>

int main() {
    struct stat st = {0};

    if (stat("/äsöme/directory", &st) == -1) {
        printf("Katalog existerar inte.\n");
    } else {
        printf("Katalog existerar.\n");
    }

    return 0;
}
```
Om katalogen inte finns, skriver koden ut "Katalog existerar inte." Om den finns, skriver den ut "Katalog existerar."

## Djupdykning
Att kolla om en katalog finns är inget nytt. Det har varit en del av Unix-baserade system ända sedan 70-talet när `stat` funktionen introducerades. Ett alternativ till `stat` är funktionen `access`, men `stat` är att föredra på grund av dess förmåga att ge mer detaljerad information om filen/katalogen.

När det kommer till implementationen, ju mer exakt vi vill vara, desto mer komplicerade blir koden. Till exempel, för att verifiera att sökvägen faktiskt leder till en katalog och inte en fil, behöver vi använda `S_ISDIR(st.st_mode)` funktionen.
 
```C
#include <sys/stat.h>
#include <stdio.h>

int main() {
    struct stat st = {0};

    if (stat("/ämazing/directory", &st) != -1) {
        if (S_ISDIR(st.st_mode)) {
            printf("Katalog existerar.\n");
        } else {
            printf("Det är en fil, inte en katalog.\n");
        }
    } else {
        printf("Sökvägen existerar inte.\n");
    }

    return 0;
}
```
## Se även
För mer information, se följande länkar:
- Man sidan för `stat`: http://man7.org/linux/man-pages/man2/stat.2.html
- C Library-funktioner: https://www.gnu.org/software/libc/manual/html_node/Testing-File-Type.html
- Diskutera och förstå `stat` och `struct stat`i detalj: https://stackoverflow.com/questions/4553012/checking-if-a-file-is-a-directory-or-just-a-file