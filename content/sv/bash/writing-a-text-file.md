---
title:                "Skriva en textfil"
date:                  2024-01-19
html_title:           "Arduino: Skriva en textfil"
simple_title:         "Skriva en textfil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skriva en textfil innebär att spara data i en fil som kan läsas som text. Programmerare gör detta för att spara konfigurationer, loggar, och användardata.

## Hur gör man:
```Bash
# Skapa en ny textfil med innehåll
echo "Hej världen!" > min_textfil.txt

# Lägg till mer text till en befintlig fil
echo "Välkommen till Bash-programmering!" >> min_textfil.txt

# Kolla innehållet i filen
cat min_textfil.txt
```
Exempelutdata:
```
Hej världen!
Välkommen till Bash-programmering!
```

## Djupdykning:
Det har länge varit standard att automatisera olika uppgifter i Unix-liknande system med shell-skript. Alternativ till att skriva textfiler i Bash inkluderar använder av scripting-språk som Python eller Perl. Kärnan i att skriva filer i Bash är omdirigeringen (>): den signalerar att Bash ska skicka utdata till filen istället för skärmen.

## Se även:
- Free Software Foundation, "Bash Reference Manual": https://www.gnu.org/software/bash/manual/
- Advanced Bash-Scripting Guide: http://www.tldp.org/LDP/abs/html/
- Stack Overflow, Bash tag for frågor och svar: https://stackoverflow.com/questions/tagged/bash
