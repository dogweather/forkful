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

## Varför

Ibland behöver vi lagra data eller information på en enkel och läsbar sätt, och därför är att skriva en textfil ett viktigt verktyg inom programmering. En textfil är också användbar för att skapa enkla konfigurationsfiler eller för att spara användarinställningar.

## Hur man gör

För att skriva en textfil behöver vi använda oss av standard C-funktioner för filhantering. Det första steget är att öppna en fil med funktionen `fopen()` och ange vilket mode filen ska öppnas i, exempelvis läge för att skriva (`"w"`). Sedan kan vi använda funktionen `fprintf()` för att skriva in önskad data i filen. När slutligen all data är skriven behöver vi stänga filen med funktionen `fclose()` för att undvika eventuella problem med filen i framtiden.

```C
// Öppna filen för skrivning
FILE *fp = fopen("exempelfil.txt", "w");

// Skriv in data i filen
fprintf(fp, "Detta är en textfil som skrivs med C-programmeringsspråket.\n");
fprintf(fp, "Här är en rad till.\n");

// Stäng filen
fclose(fp);
```

## Djupdykning

När vi skriver en textfil måste vi också ha i åtanke att filen kommer att skapas i det nuvarande arbetsmappen. Om vi vill placera filen i en annan mapp kan vi ange sökväg till filen när vi öppnar den med `fopen()`. Det är också viktigt att nämna att vi endast kan skriva in text i en textfil, alla andra datatyper måste omvandlas till text först.

## Se även

- [Officiell dokumentation för fopen()](https://www.cplusplus.com/reference/cstdio/fopen/)
- [Officiell dokumentation för fprintf()](https://www.cplusplus.com/reference/cstdio/fprintf/)
- [Officiell dokumentation för fclose()](https://www.cplusplus.com/reference/cstdio/fclose/)