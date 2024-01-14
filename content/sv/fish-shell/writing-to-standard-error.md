---
title:    "Fish Shell: Skriva till standardfel"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standardfel är en viktig del av skriptning med Fish Shell. Det gör det möjligt att tydligt markera fel och varningar i terminalen och underlättar felsökning.

## Hur man gör

För att skriva till standardfel i Fish Shell, används kommandot `echo` med flaggan `-e` för att tolka escape-sekvenser och flaggan `&2` för att skicka utskriften till standardfel. Exempelvis:

```Fish Shell
echo -e "Detta är ett felmeddelande" >&2
```

Detta kommer att skriva ut det givna meddelandet till standardfel. I Fish Shell, kan man även använda sig av `echo` kommandot utan flaggor, då standardutskriften är satt till standardutgången (vanligtvis terminalen). Exempelvis:

```Fish Shell
echo "Detta är ett meddelande"  
```

Detta kommer att skriva ut meddelandet till standardutgången. 

## Djupdykning

Det finns flera olika sätt att skriva till standardfel i Fish Shell. Utöver `echo` kommandot, kan man även använda sig av `printf` kommandot tillsammans med `$stderr` variabeln för att skriva till standardfel. Exempelvis:

```Fish Shell
printf "Detta är ett meddelande" > $stderr  
```

Man kan även använda sig av rörlig syntax för att skicka data direkt till standardfel. Exempelvis:

```Fish Shell
ls -l /path/to/nonexistent/directory 2> $stderr
```

Detta kommer att skicka eventuella felmeddelanden från `ls` kommandot till standardfel. 

## Se även

- Fish Shell dokumentation för `echo`: https://fishshell.com/docs/current/cmds/echo.html
- Fish Shell dokumentation för `printf`: https://fishshell.com/docs/current/cmds/printf.html 
- Fish Shell dokumentation för rörlig syntax: https://fishshell.com/docs/current/tutorial.html#set-stderr