---
title:    "Bash: Skriva till standard fel"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/bash/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att kunna skriva till standard error är en viktig del av Bash-programmering. Det gör det möjligt för utvecklare att fånga och hantera felmeddelanden som uppstår under körning av sina program.

## Hur man gör det
För att skriva till standard error i Bash, används kommandot `>&2` tillsammans med `echo`. Detta skickar utdatan från `echo` till standard error istället för standard output.

Ett exempel på kod:
```Bash
echo "Detta är ett felmeddelande" >&2
```

Output:
```
Detta är ett felmeddelande
```

## Deep Dive
När ett program körs i Bash, finns det tre standard strömmar av data: standard input, standard output och standard error. Standard input är där användaren kan skicka in data, standard output är där programmet skriver ut sina resultat, och standard error är där felmeddelanden och andra felaktiga utskrifter skickas till.

Genom att skriva till standard error, kan utvecklare separera felmeddelanden från vanliga resultat, vilket gör det lättare att felsöka och hantera eventuella problem i sina program. Dessutom är det möjligt att omdirigera standard error till en annan fil för att spara felmeddelanden för senare analys.

Det är också viktigt att använda `>&2` istället för bara `>`, eftersom `>` bara omdirigerar standard output och inte standard error.

## Se också
- [Bash Guide for Beginners (på engelska)](http://www.tldp.org/LDP/Bash-Beginners-Guide/html/)
- [Bash Dokumentation (på engelska)](https://www.gnu.org/software/bash/manual/bash.html)
- [Shell Scripting Tutorial (på engelska)](https://www.shellscript.sh/index.html)