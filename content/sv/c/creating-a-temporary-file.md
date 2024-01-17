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

## Vad & Varför?
Skapandet av en tillfällig fil är en vanlig händelse inom programmering. En tillfällig fil är en temporär fil som skapas och används under körningen av ett program, men som sedan raderas när programmet avslutas. Detta används ofta för att lagra data eller utföra vissa operationer som inte behöver sparas permanent.

## Så här gör du:
För att skapa en tillfällig fil i C kan du använda funktionen "tmpfile()". Detta returnerar en pekare till en ny temporär fil och öppnar den för skrivning. Du kan då skriva data till filen med hjälp av till exempel funktionen "fprintf()". När programmet avslutas kommer filen automatiskt att raderas.
```
C
#include <stdio.h>
int main() {
   FILE *fp;
   fp = tmpfile();      // skapar en temporär fil
   fprintf(fp, "%s", "Hej världen!");
   fclose(fp);          // stänger filen och raderar den
   return 0;
}
```
Output:
```
$ cat /tmp/tmp.abc2468
Hej världen!
```

## Djupdykning:
Anledningen till att skapa en tillfällig fil är oftast för att spara data under körningen av ett program utan att behöva skriva till en permanent fil. Detta kan vara användbart när man vill testa eller experimentera utan att riskera permanenta förändringar på filer eller databaser.

En annan vanligt förekommande metod för att skapa tillfälliga filer är genom att använda funktionen "mkstemp()". Denna funktion tillåter användaren att välja namn på den temporära filen och dess placering.

När en tillfällig fil skapas, lägger operativsystemet till en slumpmässig sträng som prefix till filnamnet. Detta garanterar att filen har ett unikt namn och undviker konflikter om flera program samtidigt skapar tillfälliga filer.

## Se även:
- [tmpfile()](https://www.tutorialspoint.com/c_standard_library/c_function_tmpfile.htm)
- [mkstemp()](https://www.tutorialspoint.com/c_standard_library/c_function_mkstemp.htm)
- [Filmanipulation i C](https://www.codementor.io/@bhidesagar551/filhantering-i-c-eenwy53q3)