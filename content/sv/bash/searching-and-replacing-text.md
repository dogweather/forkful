---
title:                "Bash: Sökning och ersättning av text"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en nödvändig färdighet när man programmerar i Bash. Det gör det möjligt för oss att effektivt ändra eller uppdatera stora mängder text på en gång.

## Hur man gör det

För att söka och ersätta text i Bash måste vi använda oss av kommandot `sed`. Nedan följer ett enkelt exempel på hur man kan ersätta alla förekomster av ordet "hej" med "tja" i en fil som heter `text.txt`:

```Bash 
sed 's/hej/tja/g' text.txt
```

I det här exemplet kommer varje förekomst av "hej" i `text.txt` att ersättas med "tja". Den modifierade texten skrivs ut i terminalen, men för att spara ändringarna i filen måste vi lägga till flaggan `-i`:

```Bash
sed -i 's/hej/tja/g' text.txt
```

På så sätt kommer `sed` att modifiera filen direkt istället för att bara visa ändringarna i terminalen.

## Djupdykning

Utöver den grundläggande syntaxen för att söka och ersätta text har `sed` flera andra funktioner som kan användas för att göra mer avancerade sökningar och ersättningar. Till exempel kan vi använda reguljära uttryck för att ersätta mer komplexa strängar.

En annan användbar funktion är flaggan `-n`, som gör att endast de rader som matchar det sökta uttrycket skrivs ut. Till exempel, om vi bara vill se rader som innehåller ordet "hej" i en fil, kan vi använda oss av följande kod:

```Bash
sed -n '/hej/p' text.txt
```

Här kommer bara rader som innehåller "hej" att skrivas ut i terminalen.

## Se även

- [Bash Tutorial: Introduction to Searching and Replacing Text](https://www.grymoire.com/Unix/Search.html)
- [Sed Command in Linux/Unix with Examples](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [The Linux Documentation Project: Sed](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_04_01.html)