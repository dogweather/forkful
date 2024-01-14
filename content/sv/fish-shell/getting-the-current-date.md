---
title:    "Fish Shell: Att få aktuellt datum"
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför
Att kunna få den aktuella datumet kan vara användbart för många olika program och skript. Det kan ge en bättre översikt över när olika händelser inträffade eller när vissa åtgärder ska utföras.

## Hur man gör
Att få det nuvarande datumet i Fish Shell är enkelt. Använd kommandot `date` följt av flaggan `+%D`. Detta kommer att ge dig ett utskrift med dag, månad och år i formatet MM/DD/YY.

```Fish Shell
date +%D
```

För att få datumet i formatet DD/MM/YY kan du använda flaggan `+%T` istället.

```Fish Shell
date +%T
```

Det finns flera olika flaggor som kan användas tillsammans med `date` för att få olika datumformat eller specifika datum från det förflutna. Se manualsidan för `date` för mer information.

## Djupdykning
Bakom kulisserna använder `date` kommandot operativsystemets klocka för att få den aktuella tiden och datumet. Detta gör att outputen kommer att vara baserad på vad din dator har för inställningar för datum och tid.

För att ändra tidszonen som används för `date` kommandot kan du använda flaggan `-u` för att få UTC-tiden, eller kolla på inställningarna för din dator för att ändra tidszonen.

## Se även
- [Fish Shell kommandon](https://fishshell.com/docs/current/cmds.html)
- [`date` kommandots manualsida](https://fishshell.com/docs/3.3/cmds/date.html)