---
title:                "Fish Shell: Skriver till standardfel"
simple_title:         "Skriver till standardfel"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Varför

Att skriva till standard error är en viktig del av programmering för att kunna skriva ut felmeddelanden som är lättlästa för användare.

## Hur man gör det

För att skriva till standard error i Fish Shell använder man sig av kommandot `echo` med flaggan `-e` för att kunna tolka escape-tecken. Sedan skriver man ut sitt felmeddelande inom citattecken och använder `2>1` för att skicka output till standard error. Se nedan för en kodexempel och den resulterande outputen:

```
Fish Shell kodexempel:
```fish
echo -e "Ett fel inträffade" 2>&1
```

Output:
```
Ett fel inträffade
```

I detta exempel kommer “Ett fel inträffade” att skrivas ut till standard error, eftersom vi har använt `2>&1` för att skicka output dit istället för till standard out.

## Fördjupning

Att skriva till standard error är användbart när man vill separera felmeddelanden från annan output. Det hjälper också användare att lättare identifiera och felsöka problem. För att skriva till standard error i andra språk som Bash eller Zsh använder man sig av `1>&2` istället, eftersom standard error och standard out är omvända i dessa shells.

## Se även

- [Fish Shell dokumentation](https://fishshell.com/docs/current/cmds/echo.html)
- [Mer om standard error och standard out](https://stackoverflow.com/questions/34977294/cannot-display-error-message-in-fish-shell)
- [Mer om escape-tecken](https://www.golinuxcloud.com/echo-without-escaping-leds-and-codes-in-shell/)