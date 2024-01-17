---
title:                "Skapa en temporär fil"
html_title:           "Bash: Skapa en temporär fil"
simple_title:         "Skapa en temporär fil"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Skapandet av en temporär fil är en vanlig praxis bland programmerare. Det är en fil som tillfälligt används för att lagra data eller resultat under körning av ett program. Det görs för att undvika att permanent skriva till en befintlig fil och riskera att överskrida dess storlek eller ändra dess innehåll.

## Så här:
För att skapa en temporär fil i Bash kan du använda kommandot `mktemp` och ange ett prefix för filnamnet:
```
Bash $ mktemp output.txt
```
Detta kommer att skapa en fil med namnet `output.txt5sPz20` där de sista sex tecknen är en slumpmässig sträng som läggs till som en säkerhetsåtgärd för att undvika filnamnskollisioner. Om du vill skriva till filen kan du använda `echo` kommandot:
```
Bash $ echo "Det här är innehållet" > output.txt5sPz20
```
Du kan också använda `>` för att omdirigera ett kommando eller en fil till den temporära filen:
```
Bash $ ls > output.txt5sPz20
```
För att ta bort den temporära filen och dess innehåll kan du använda kommandot `rm`:
```
Bash $ rm output.txt5sPz20
```

## Djupdykning:
Skapandet av temporära filer är en vanlig praxis i många programmeringsspråk, inte bara Bash. Det används för att undvika att skriva till befintliga filer och riskera att skriva över viktig data eller ändra filens innehåll. Det finns också alternativ till `mktemp` som till exempel `tempfile` för att skapa temporära filer. Dessutom kan du också skapa en temporär fil i ett annat filsystem genom att ange dess sökväg. Vid skapandet av en temporär fil i Bash skapas den som standard med tillståndet 600, vilket innebär att endast användaren som skapat filen har åtkomst till den.

## Se även:
- https://linux.die.net/man/1/mktemp
- https://www.gnu.org/software/bash/manual/bash.html#Redirections
- https://www.linuxjournal.com/content/working-temporary-files-bash