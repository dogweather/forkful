---
title:                "Skrivning till standardfel"
html_title:           "Fish Shell: Skrivning till standardfel"
simple_title:         "Skrivning till standardfel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför
Att skriva till standard error kan vara en användbar programmeringsteknik för att hantera fel eller avvikelser som kan uppstå under exekvering av kommandon via Fish Shell. Det kan hjälpa till att skapa en mer användarvänlig upplevelse för användaren.

## Så här gör du
Om du vill skriva till standard error inom Fish Shell, kan du använda kommandot `echo`. Till exempel, om du vill skriva ut ett felmeddelande med meddelandet "Ogiltigt kommando", kan du använda följande kommando:

```Fish Shell
echo "Ogiltigt kommando" >&2
```

På detta sätt kommer meddelandet att skrivas till standard error istället för standard output. Detta kan vara särskilt användbart om du vill skilja mellan olika typer av meddelanden, t.ex. felmeddelanden och vanliga utskrifter.

## Djupdykning
När du skriver till standard error inom Fish Shell, används symbolen `>&2` för att omdirigera utdata från standard output till standard error. Detta kan också användas tillsammans med andra kommandon för att hantera utskrifter. Till exempel, om du vill skriva ut alla filnamn i en mapp, men också skriva ut felmeddelanden om en fil saknas, kan du använda följande kommando:

```Fish Shell
ls *.txt 2>&1 | grep "Filen finns inte"
```

Detta kommer att skriva ut en lista över alla filer som matchar namnmönstret `*.txt`, men också skriva ut eventuella felmeddelanden om filer som saknas. På detta sätt kan du använda `echo` och `>&2` tillsammans med andra kommandon för att hantera utdata på ett mer effektivt sätt.

## Se också
- [Fish Shell dokumentation](https://fishshell.com/docs/current/)
- [Fish Shell GitHub repository](https://github.com/fish-shell/fish-shell)
- [Learning Fish Shell](https://fishshell.com/docs/current/tutorial.html)