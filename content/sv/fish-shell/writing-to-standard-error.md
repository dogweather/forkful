---
title:                "Fish Shell: Skriva till standardfel"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är viktigt för att kunna hantera felmeddelanden och felsökning i Fish Shell. Genom att skicka information till standardfel istället för standardutdata kan användaren enkelt spåra fel och identifiera problemkällan.

## Så här gör du

För att skriva till standardfel i Fish Shell, använd kommandot `>&2`. Detta kommer att skicka all information som följer efter kommandot till standardfel istället för standardutdata. Detta är särskilt användbart när du vill skicka utdata till ett fil eller rör utan att störa andra kommandon eller skapa onödig output.

Ett enkelt exempel på hur du kan använda `>&2` är att skicka ett felmeddelande när ett kommando misslyckas. Till exempel:

```Fish Shell
ls -l non_existent_file >&2
```

I detta fall kommer `ls`-kommandot att misslyckas eftersom filen inte finns, och felmeddelandet kommer att skickas till standardfel istället för att visas i terminalen.

## Djupdykning

Att skicka information till standardfel kan också vara användbart när man använder pipelining och vill se till att ett felmeddelande inte påverkar resultatet av de andra kommandon i pipen.

Ett annat användningsområde för att skriva till standardfel är att använda `>&2` tillsammans med `echo`-kommandot för att skapa en loggfil som endast innehåller felmeddelanden. Till exempel:

```Fish Shell
echo "Error while running command." >&2 >> error.log
```

Denna rad kommer att skriva ett felmeddelande till filen "error.log" istället för att visa det i terminalen.

## Se även

Här är några andra användbara resurser för att lära dig mer om Fish Shell:

- Fish Shell officiell hemsida: https://fishshell.com/
- Fish Shell Wiki: https://github.com/fish-shell/fish-shell/wiki
- Fish Shell forum: https://fishshell.com/forum/