---
title:                "Att skapa en tillfällig fil"
html_title:           "Bash: Att skapa en tillfällig fil"
simple_title:         "Att skapa en tillfällig fil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skapa en temporär fil innebär att programmeraren ger programmet ett utrymme för att lagra data tillfälligt under körning. Detta används ofta för att minska minnesanvändningen och hantera stora datamängder som kanske inte behöver lagras permanent.

## Hur du gör det:

I C programmeringsspråk, vi använder funktionen `tmpfile()` för att skapa en temporär fil. Koden ser ut något så här:

```C 
#include <stdio.h>

int main () {
   FILE *fp;

   fp = tmpfile();
   
   fprintf(fp, "%s", "En tillfällig sträng.");
   rewind(fp);
   
   char buff[50];
   
   while(fgets(buff, sizeof(buff), fp) != NULL){
       printf("%s", buff);
   }
   
   fclose(fp);
   
   return(0);
}
```
Efter att ha kört programmet kommer du att se utdata: `En tillfällig sträng.`

## Djup dykning:

Historiskt sett användes temporära filer för att hantera minnesbegränsningar på äldre datorsystem. Idag hjälper de till att hantera stora mängder data i program som inte bör ändra de permanenta lagrade data, såsom loggvisare eller textredigeringsprogram.

Det finns alternativ till `tmpfile()`, inklusive `mkstemp()` och `tmpnam()`. Valet mellan dessa beror oftast på de specifika användningsfallen och systemets begränsningar.

Implementeringen av `tmpfile()` skapar en unik fil med ett unikt namn i systemets standardkatalog för temporära filer. Denna fil är öppen för skrivning och raderas automatiskt när filen stängs eller programmet avslutas.

## Se även:

För mer information och detaljerad förklaring, här finns några användbara länkar:

- "Temporary File Functions" från GNU Library: www.gnu.org/savannah-checkouts/gnu/libc/manual/html_node/Temporary-Files.html
- "tmpfile function" från Microsoft Developer Network: https://docs.microsoft.com/en-us/cpp/c-runtime-library/reference/tmpfile-tmpfile-tmpfile-s-tmpfile-s?redirectedfrom=MSDN&view=msvc-160