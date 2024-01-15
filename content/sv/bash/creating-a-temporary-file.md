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

## Varför
Skapandet av temporära filer är en viktig del av Bash-programmering eftersom det tillåter oss att enkelt lagra och manipulera data under körningen av ett skript utan att permanent påverka filsystemet.

## Så här gör du
För att skapa en temporär fil i Bash använder vi kommandot "mktemp" tillsammans med flaggan "-p" för att specificera en mapp där filen ska skapas. Här är ett exempel på hur det skulle kunna se ut i ett skript:

```Bash
#!/bin/bash

TEMP_FILE=$(mktemp -p ~/temp)

echo "Det här är en temporär fil" >> $TEMP_FILE

cat $TEMP_FILE
```

Det första steget är att definiera variabeln "TEMP_FILE" och tilldela den resultatet av "mktemp" kommandot som använder "-p" flaggan för att skapa filen i mappen "temp" i användarens hemkatalog. Sedan använder vi "echo" kommandot för att skriva en rad till filen och slutligen använder vi "cat" kommandot för att skriva ut innehållet i filen. Resultatet skulle vara:

```
Det här är en temporär fil
```

Vi kan också använda kommandon som "mktemp -u" för att skapa en unik temporär fil och "mktemp -t" för att inkludera ett prefix i filnamnet.

## Vertikal dyk
Skapandet av temporära filer är en viktig del av Bash-programmering eftersom vi ibland behöver lagra och hantera data som inte kan sparas permanent i filsystemet. Men det är också värt att nämna att det även finns andra sätt att skapa temporära filer, som att använda ">>" operatorn för att lägga till innehållet i en befintlig fil eller att använda "trap" kommandot för att rensa bort temporära filer när skriptet avslutas.

## Se även
[Filhantering i Bash](https://www.tecmint.com/file-management-in-linux/)  
[Mktemp command in Bash](https://www.geeksforgeeks.org/mktemp-command-in-linux-with-examples/)