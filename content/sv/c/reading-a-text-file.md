---
title:                "Läsa en textfil"
html_title:           "Fish Shell: Läsa en textfil"
simple_title:         "Läsa en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Läsa en textfil i programmering innebär att öppna och tolka innehållet i filen med hjälp av ett programmeringsspråk, som C. Detta är viktigt för programmerare eftersom det tillåter oss att importera och bearbeta data från externa källor.

## Hur du gör:

Låt oss göra det grundläggande med en enkel kod i C för att läsa en textfil. Ersätt bara `FILE_NAME.txt` med din riktiga filnamn.

```C
#include <stdio.h>

int main() {
  FILE *file = fopen("FILE_NAME.txt", "r");
  char ch;

  if (file == NULL) {
    printf("Kunde inte öppna filen.\n");
    return 0;
  }

  while((ch = fgetc(file)) != EOF){
    printf("%c", ch);
  }

  fclose(file);
  return 0;
}
```

När du kör koden ovan kommer du att se innehållet i din textfil skriven på din skärm.

## Djupdykning

Ursprungligen skapades C för och av operativsystemet Unix under 1970-talet. Teknikerna för att läsa och skriva till filer har varit centralt sedan dess.

Det finns flera alternativ och metoder för att hantera textfiler i C. Till exempel genom att använda `fgets()` funktionen, som låter dig läsa filen radvis.

Implementationen av dessa funktioner kan variera något beroende på operativsystem. De flesta levande system kommer att följa POSIX-standarden, men det är inte alltid fallet (t.ex. Windows).

## Se även

För mer om läsa och skriva till filer i C:
- [Tutorial på Cprogramming.com](https://www.cprogramming.com/tutorial/cfileio.html)
- [Filen Hantering i C från GeeksForGeeks](https://www.geeksforgeeks.org/basics-file-handling-c/)
- [C Fil I/O från tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)

För mer om historia och standarder:
- [Historien om C på Wikipedia](https://en.wikipedia.org/wiki/C_(programming_language))
- [POSIX standarden på opengroup.org](https://pubs.opengroup.org/onlinepubs/9699919799/)