---
title:                "Skriva en textfil"
html_title:           "C: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

"## Vad & Varför?"
Att skriva en textfil är en vanlig uppgift för programmerare. Det innebär att man skriver en fil med textinformation som kan läsas och tolkas av datorn. Textfiler används ofta för att lagra och hantera data, som till exempel användarinformation eller textfiler som behövs för att köra ett program.

"## Hur gör man:"
För att skriva en textfil i C, behöver vi använda oss av några grundläggande funktioner som hör till C standardbiblioteket. Här är ett exempel på kod som skapar en ny textfil och skriver texten "Hej världen!" i filen:
 
```C
#include <stdio.h> // inkluderar standardbiblioteket för att få tillgång till grundläggande funktioner
 
int main() {
  FILE *fp; // skapar en pekare till filen
  fp = fopen("exempel.txt", "w"); // öppnar en ny fil med namnet "exempel.txt" i skrivläge
  fprintf(fp, "Hej världen!"); // skriver texten i filen
  fclose(fp); // stänger filen
  return 0;
}
```

En ny textfil med namnet "exempel.txt" kommer nu att skapas i samma mapp där du kör programmet. Om du öppnar filen kommer du att se att texten "Hej världen!" har skrivits in.

"## Djupdykning:"
Att kunna skriva en textfil är en viktig del av programmering eftersom det låter oss spara och hantera information på ett effektivt sätt. Textfiler används ofta för att konfigurera program, spara användarinställningar och för att visa utdata från ett program.

Det finns också andra sätt att hantera information i C, som till exempel att använda C structs eller dynamiska datastrukturer som arrays och listor. Men textfiler är ofta enklare att hantera och använda i mer än en programmeringsspråk, vilket är en stor fördel.

När man skriver en textfil är det också viktigt att veta att det finns olika lägen för att skriva till filen. I vårt exempel ovan använde vi "w"-läget vilket betyder att vi öppnade filen i skrivläge och skrev över allt som fanns i filen tidigare. Det finns också "a"-läget som lägger till ny information i filen utan att skriva över den befintliga informationen.

"## Se även:"
Mer information om hur man skriver en textfil i C kan hittas i C standardbibliotekets dokumentation [här](https://www.cplusplus.com/reference/cstdio/fopen/). Du kan också läsa mer om alternative sätt att hantera information i C, som till exempel structs, [här](https://www.tutorialspoint.com/cprogramming/c_structures.htm).