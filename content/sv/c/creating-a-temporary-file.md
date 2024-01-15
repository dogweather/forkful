---
title:                "Skapa en temporär fil"
html_title:           "C: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Varför
Varför skulle någon vilja skapa en temporär fil när man programmerar i C? Det finns flera användningsområden för temporära filer, bland annat för att lagra temporära data eller för att testa kod innan den implementeras permanent.

## Så här gör du
Det första steget för att skapa en temporär fil i C är att inkludera header-filen "stdio.h". Sedan använder du funktionen "tmpfile()" för att skapa filen och får tillbaka en pekare till filen.

```C
#include <stdio.h>

int main() {
    FILE* temp_file = tmpfile();

    if (temp_file == NULL) {
        printf("Kunde inte skapa den temporära filen!");
        return 1;
    }

    fprintf(temp_file, "Det här är en temporär fil!");
    fclose(temp_file);

    return 0;
}
```

När du är klar med filen måste den stängas med "fclose()" för att frigöra minnet. Om du vill läsa från en temporär fil istället för att skriva till den, kan du använda funktionen "tmpnam()" för att skapa en temporär fil med ett namn och sedan öppna den med "fopen()".

```C
#include <stdio.h>

int main() {
    char* temp_name = tmpnam(NULL);
    FILE* temp_file = fopen(temp_name, "r");

    if (temp_file == NULL) {
        printf("Kunde inte öppna den temporära filen!");
        return 1;
    }

    char buffer[50];
    while (fgets(buffer, sizeof(buffer), temp_file) != NULL) {
        printf("%s", buffer);
    }

    fclose(temp_file);

    return 0;
}
```

## Djupdykning
Förutom "tmpfile()" och "tmpnam()", finns det också andra funktioner för att skapa temporära filer i C. Till exempel kan "mkstemp()" användas för att skapa en temporär fil med en unik filnamnsprefix, medan "mkdtemp()" används för att skapa en temporär katalog. Det finns också möjlighet för att skapa en temporär fil i en specifik katalog istället för den vanliga temporära mappen, med funktionen "tmpfile64()". Det är viktigt att notera att namnen på temporära filer och kataloger generellt inte är garanterat att vara unika, så det är bäst att ta hänsyn till detta när du skapar temporära filer i dina program.

## Se även
- [Officiell dokumentation för "tmpfile()"](https://www.gnu.org/software/libc/manual/html_node/Opening-Temporary-Files.html)
- [En guide till att hantera temporära filer i C](https://www.educative.io/edpresso/how-to-create-temporary-files-in-c)
- [Kodexempel för hantering av temporära filer i C](https://www.programiz.com/c-programming/examples/temporary-file)