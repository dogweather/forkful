---
title:    "Fish Shell: Utskrift av felsökningsutdata"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Varför

Att skriva ut debug-utmatning kan vara en användbar metod för att felsöka problem i din Fish Shell-kod. Det kan hjälpa dig att förstå hur din kod fungerar och hitta eventuella fel eller buggar. 

## Så här gör du

För att skriva ut debug-utmatning i Fish Shell, använd kommandot `echo`. Du kan skriva ut variabler, textsträngar eller resultatet av en kommandokörning. Här är ett exempel på hur du skriver ut värdet av variabeln `name`:

```Fish Shell
echo $name
```

Du kan också använda flaggan `-v` för att skriva ut variablers namn och värden:

```Fish Shell
echo -v name
```

För att skriva ut en textsträng, använd citattecken runt strängen:

```Fish Shell
echo "Detta är en textsträng."
```

Slutligen kan du använda kommandot `set -x` för att aktivera debug-mode i Fish Shell. Detta kommer att skriva ut alla körda kommandon och resultatet av dem. Glöm inte att stänga av debug-läget när du är klar genom att köra `set -x off`.

## Djupdykning

Att skriva ut debug-utmatning är särskilt användbart när du har komplexa skript eller när du arbetar med flera variabler och vill se hur de ändras under körning. Det kan också vara användbart att använda verktyg som `less` för att enklare navigera genom stora utskrifter. Du kan använda `| less` efter ditt `echo`-kommando för att se utmatningen i mindre delar.

## Se även

- [Fish Shell-dokumentation](https://fishshell.com/docs/current/)
- [En introduktion till Fish Shell](https://dev.to/rstacruz/an-introduction-to-fish-shell-3m8n)