---
title:                "Använda en interaktiv skal (REPL)"
aliases:
- /sv/powershell/using-an-interactive-shell-repl/
date:                  2024-01-26T04:16:48.694052-07:00
model:                 gpt-4-0125-preview
simple_title:         "Använda en interaktiv skal (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/powershell/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Den interaktiva skalet, eller Read-Eval-Print Loopen (REPL), låter dig skriva PowerShell-kommandon och få omedelbar återkoppling. Programmerare använder den för att snabbt testa kodsnuttar, felsöka eller lära sig nya kommandon utan att skriva ett helt skript.

## Hur man gör:
Starta PowerShell och du är i REPL. Prova Cmdleten `Get-Date`:

```PowerShell
PS > Get-Date
```

Du bör se det aktuella datumet och tiden som utmatning:

```PowerShell
Onsdag, 31 mars 2023 12:34:56 PM
```

Nu, kedja kommandon. Låt oss sortera processer efter minnesanvändning:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

Detta ger ut de fem främsta processerna efter storlek på arbetsuppsättningen (minnesanvändning).

## Djupdykning
PowerShell:s REPL har sina rötter i Unix-skalet och andra dynamiska språkskal som Python. Det är en enanvändar-, interaktiv kommandotolk. Till skillnad från ett kompilerat språk där du skriver hela applikationer och sedan kompilerar, låter en REPL-miljö dig skriva och köra kod en rad i taget. PowerShell stöder också skriptkörning för större uppgifter.

Alternativ för Windows inkluderar Kommandotolken eller andra språkspecifika REPL:s som IPython. I Unix/Linux-världen tjänar skal som bash eller zsh en liknande funktion.

PowerShell:s implementering använder en värdapplikation för att köra skalet. Även om PowerShell.exe i Windows är den vanligaste, kan andra som Integrated Scripting Environment (ISE) eller Visual Studio Code:s integrerade terminal också fungera som värd.

## Se även
- [Om PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
