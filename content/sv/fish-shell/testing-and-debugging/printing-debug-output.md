---
aliases:
- /sv/fish-shell/printing-debug-output/
date: 2024-01-20 17:52:31.212975-07:00
description: "Debugutskrifter \xE4r sm\xE5 meddelanden kodare infogar f\xF6r att sp\xE5\
  ra vad ett program g\xF6r. De \xE4r superhj\xE4lpsamma f\xF6r att f\xF6rst\xE5 kodens\
  \ fl\xF6de och fixa buggar."
lastmod: 2024-02-18 23:08:52.210039
model: gpt-4-1106-preview
summary: "Debugutskrifter \xE4r sm\xE5 meddelanden kodare infogar f\xF6r att sp\xE5\
  ra vad ett program g\xF6r. De \xE4r superhj\xE4lpsamma f\xF6r att f\xF6rst\xE5 kodens\
  \ fl\xF6de och fixa buggar."
title: "Skriva ut fels\xF6kningsdata"
---

{{< edit_this_page >}}

## What & Why?
Debugutskrifter är små meddelanden kodare infogar för att spåra vad ett program gör. De är superhjälpsamma för att förstå kodens flöde och fixa buggar.

## How to:
I Fish använder du `echo` eller `printf` för att skriva ut debuginfo. Enkelt och rakt på sak. Här är exempel:

```Fish Shell
# Använd echo för att skriva ut en enkel textsträng
echo "Här börjar vi debugga"

# Lägg till en variabel i debugutskriften
set var "Hemlig kod"
echo "Variabelvärdet är: $var"

# Mer avancerat, med printf för formatering
set nummer 42
printf "Debug: numret är %d\n" $nummer
```

Exempel på utskrift:

```
Här börjar vi debugga
Variabelvärdet är: Hemlig kod
Debug: numret är 42
```

## Deep Dive
`echo` är det simplaste kommandot och har funnits sedan urminnes tider. Då vi snackar Fish så är syntaxen nästan identisk med andra shells. `printf` däremot är en lite nyare grej som ger mer kontroll över formatet. I Fish är styrkan att man kan göra funktioner för återkommande debugmönster - allt för att göra livet smidigare.

Alternativ till `echo` och `printf`? Jo, vissa kodare använder verktyg som `stderr` för att separera vanlig output från felsökningsmeddelanden. Så här:

```Fish Shell
echo "Detta är en vanlig meddelande" 
echo "Här kommer ett debugmeddelande" >&2
```

## See Also
- Fish Shell's officiella dokumentation om 'echo': https://fishshell.com/docs/current/cmds/echo.html
- En guide till 'printf' i Fish: https://fishshell.com/docs/current/cmds/printf.html
- Felsökning och debuggens konst: https://fishshell.com/docs/current/index.html#debugging
