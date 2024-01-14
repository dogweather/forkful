---
title:    "Fish Shell: Skapa en temporär fil"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför skapa en temporär fil?
Att skapa temporära filer är ett användbart verktyg inom programmering, särskilt när du arbetar med datahantering. Det kan hjälpa till att spara och hantera data temporärt innan det slutligen sparas permanent. Det kan också hjälpa till att lösa problem med filåtkomst och återställa data om programmet avslutas oväntat.

## Hur man skapar en temporär fil i Fish Shell
Att skapa en temporär fil i Fish Shell är en enkel process. Du kan använda kommandot `mktemp` för att skapa en temporär fil med ett slumpmässigt namn. Du kan inte bara skapa en fil, utan också en katalog, med detta kommando. Här är ett exempel på hur man skapar en temporär fil med `mktemp`:

```
fish
mktemp
```

Output: `/var/folders/92/3_zsk1b12sz3cl4_l_zb007m0000gr/T/tmp.9k597h`

## Deep Dive: Skapa en temporär fil med förlängning
En införlivad funktion i Fish Shell är möjligheten att skapa en temporär fil med en specifik filändelse. Detta kan vara användbart när du har olika typer av data som behöver hanteras på olika sätt. Här är hur du skapar en temporär fil med förlängningen `.txt`:

```
fish
mktemp --suffix=".txt"
```

Output: `/var/folders/92/3_zsk1b12sz3cl4_l_zb007m0000gr/T/tmp.ylrxba.txt`

## Se även
- [Fish Shell dokumentation](https://fishshell.com/docs/current/cmds/mktemp.html)
- [Skapa och hantera temporära filer i Fish Shell](https://www.howtogeek.com/439807/how-to-use-the-mktemp-command-on-linux/)
- [Hantera temporära filer i Shell Scripting](https://linuxhint.com/temporary_files_shell_scripting/)