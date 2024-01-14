---
title:                "Fish Shell: Utskrift av felsökningsutdata"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## Varför

Det kan vara ovärderligt att kunna skriva ut debuggutdata när det kommer till att programmera i Fish Shell. Det kan hjälpa dig att hitta fel och förstå hur din kod fungerar. I denna bloggpost kommer vi att utforska hur du kan göra detta.

## Hur man gör det

Det första du behöver göra är att använda kommandot `echo` för att skriva ut text i terminalen. I `Fish Shell` kan du använda `debug`-kommandot för att bara skriva ut debuggutdata när `debug`-flaggan är satt. Det ser ut något i stil med detta:

```
Fish Shell

echo "Debuggutdata"
debug "Denna kommer bara att skrivas ut om debugflaggan är satt"
```

Om du kör detta kommer endast texten "Debuggutdata" att skrivas ut. Om du vill se den andra texten måste du sätta flaggan vid körningen `fish -d 3 script.fish`, där `3` är loggnivån för debuggutdata.

## Djupdykning

Nu när du vet hur man skriver ut debuggutdata i Fish Shell, låt oss titta på hur du kan använda det i praktiken. Genom att sätta debuggutdata i olika delar av din kod, kan du se vilka delar som körs och hur. Detta kan hjälpa dig att hitta fel och förbättra din kod.

Några bra användningsområden för debuggutdata är:

- Att kontrollera värden på variabler
- Att förstå hur en loop körs
- Att se hur en funktion anropas och blir returnerad

Kom ihåg att ta bort eller kommentera ut debuggutdata innan du publicerar din kod, eftersom det inte är meningen att de ska finnas där i produktionen.

## Se även

Här är några länkar som kan hjälpa dig att lära dig mer om debuggutdata i Fish Shell:

- [Fish Shell - Debug command](https://fishshell.com/docs/current/commands.html#debug)
- [Shell scripting in fish](https://www.linux.com/tutorials/shell-scripting-in-fish/)
- [Debugging a Shell Script](https://www.tutorialspoint.com/unix_commands/echo.htm) (på engelska) 

Förhoppningsvis kommer denna information att hjälpa dig att använda debuggutdata för att förbättra din kodning i Fish Shell. Lycka till på din programmeringsresa!