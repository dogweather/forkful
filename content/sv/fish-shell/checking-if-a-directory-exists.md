---
title:                "Kontrollera om en katalog finns"
date:                  2024-01-20T14:56:10.320092-07:00
simple_title:         "Kontrollera om en katalog finns"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en mapp finns är ett sätt att se till att en väg pekar mot en faktisk mapp i filsystemet. Programmerare gör det för att undvika fel när de försöker läsa från eller skriva till mappar som inte finns.

## Hur gör man:
Kontrollera om en mapp finns i Fish genom att använda `test` kommandot.

```Fish Shell
if test -d /din/mapp/väg
    echo "Mappen finns!"
else
    echo "Mappen finns inte!"
end
```

Testa om en hem-mapp finns:

```Fish Shell
if test -d ~/Documents
    echo "Ditt 'Documents' katalog finns!"
else
    echo "Ditt 'Documents' katalog finns inte!"
end
```

Sample Output:
```
Ditt 'Documents' katalog finns!
```

## Fördjupning
Förr i tiden använde många `if [ -d /path/to/dir ]`, men i Fish är `test` inbyggt och behöver inte hakparanteser. Alternativ för att kontrollera mappar inkluderar `stat` och direkt-listning med `ls`, men dessa kan vara överdrivet komplicerade till detta enkla behov. `test -d` kontrollerar om sökvägen existerar och är en mapp, vilket är implementationen som främst används idag för dess enkelhet och direktinformation.

## Se även
- Fish dokumentation om `test` kommandot: https://fishshell.com/docs/current/commands.html#test
- Filhantering i Fish: https://fishshell.com/docs/current/tutorial.html#tut_file_operations
- Manualsidan för `test` (inbyggt): https://fishshell.com/docs/current/cmds/test.html
