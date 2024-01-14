---
title:    "Fish Shell: Läsa kommandoradsargument"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Varför
Command line-argument är en viktig del av programmering eftersom det ger möjlighet att interagera med programmet från kommandoraden. Att kunna läsa kommandoradsargument korrekt är en viktig färdighet för alla som arbetar med Fish Shell.

## Hur man läser kommandoradsargument i Fish Shell
Läsning av kommandoradsargument i Fish Shell kan göras enkelt med hjälp av inbyggda funktioner och variabler. Här är ett exempel på hur man kan läsa ett kommandoradsargument som representerar ett namn:

```Fish Shell
set name $argv[1]
echo "Hej $name!" 
```

Om man till exempel kör skriptet "hello.fish" med kommandot `fish hello.fish John`, kommer resultatet att bli `Hej John!`.

`$argv` är en inbyggd Fish Shell-array som innehåller alla kommandoradsargument som har skickats in när skriptet körs. Indexeringen börjar på 1, så för att läsa första argumentet används `$argv[1]`.

## Djupdykning
När man arbetar med kommandoradsargument är det viktigt att tänka på hantering av fel. Om man till exempel förväntar sig ett visst antal argument men inte får det, kan detta orsaka problem i skriptet. Därför är det viktigt att alltid kontrollera antalet argument innan man läser in dem.

Man kan även använda alternativa syntaxer för att läsa argument som innehåller specialtecken, till exempel `set name $arg1` för att läsa argumentet `$1`.

Det är också möjligt att kombinera flera olika kommandoradsargument och hantera dem på olika sätt i skriptet, beroende på vilka användaren väljer att skicka in.

## Se även
- [Fish Shell dokumentation om kommandoradsargument](https://fishshell.com/docs/current/cmds/set.html#synopsis)
- [En tutorial om hantering av kommandoradsargument i Fish Shell](https://www.linode.com/docs/guides/writing-fish-shell-scripts/#command-line-arguments)
- [Andra inbyggda Fish Shell-variabler för kommandoradsargument](https://fishshell.com/docs/current/cmds/set.html#set)