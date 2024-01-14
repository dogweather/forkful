---
title:                "Fish Shell: Läsning av kommandoradargument"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Varför

Svart att navigera och använda kommandoraden kan vara en skrämmande uppgift, men med hjälp av Fish Shell-programmering kan det bli enkelt och effektivt. Läs vidare för att lära dig hur du läser kommandoradsargument och effektiviserar ditt arbetsflöde.

# Hur man gör

```Fish Shell``` är ett kraftfullt verktyg som ger användare möjlighet att göra avancerade och komplicerade uppgifter direkt från kommandoraden. För att läsa kommandoradsargument använder vi $argv-variabeln. Den här variabeln innehåller alla argument som matas in i kommandoraden när en fil exekveras. 

För att visa de tillagda argumenten, skriver du ```echo $argv```. Detta ger dig en lista med alla angivna argument, separerade med mellanslag. 

Om du vill ha mer exakt information om ett specifikt argument, kan du använda $argv[1] för att välja det första argumentet. Om du till exempel vill veta vad det andra argumentet är, skriver du: ```echo $argv[2]```.

# Djupdykning

Om du vill göra ett mer komplicerat kommando med hjälp av kommandoradsargument, kan du använda $argv-längden för att kontrollera antalet argument som matas in. Till exempel, om du vill skapa en enskild fil med namnet för det första argumentet, kan du skriva följande kod:

```Fish Shell
if not set -q 'argv[1]'
    //Hantera en felstatus här
else 
    touch $argv[1]
```

Detta kommer att skapa en ny fil med det första argumentet som namn. Om inte något argument är angivet, kommer det att resultera i en felstatus.

# Se även

- [Officiell Fish Shell dokumentation] (https://fishshell.com/docs/current/)

- [Fish Shell-tutorials på YouTube] (https://www.youtube.com/playlist?list=PL7e8VJ_ZN6epq-oiYOJ7MfiSAc5SZ7ror)

- [Fish Shell-stödforum] (https://github.com/fish-shell/fish-shell/issues)