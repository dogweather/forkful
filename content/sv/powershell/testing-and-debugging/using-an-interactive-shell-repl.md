---
date: 2024-01-26 04:16:48.694052-07:00
description: "Hur man g\xF6r: Starta PowerShell och du \xE4r i REPL. Prova Cmdleten\
  \ `Get-Date`."
lastmod: '2024-03-13T22:44:38.127521-06:00'
model: gpt-4-0125-preview
summary: "Starta PowerShell och du \xE4r i REPL."
title: "Anv\xE4nda en interaktiv skal (REPL)"
weight: 34
---

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
