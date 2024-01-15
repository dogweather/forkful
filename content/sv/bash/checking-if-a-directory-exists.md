---
title:                "Kontrollera om en mapp finns"
html_title:           "Bash: Kontrollera om en mapp finns"
simple_title:         "Kontrollera om en mapp finns"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp existerar är en vanlig uppgift när man skriver Bash-skript. Det kan till exempel användas för att avgöra om en viss installation krävs eller för att kontrollera tillgången till en nödvändig resurs.

## Så här gör du

För att kontrollera om en mapp existerar kan du använda kommandot "test" med flaggan "-d" som står för "directory". Detta kommer att returnera ett "true" värde om mappen existerar och ett "false" värde om den inte gör det.

```Bash
if test -d /path/to/directory; then
    echo "Mappen existerar!"
else
    echo "Mappen existerar inte."
fi
```

Om du bara vill ha ett enklare svar, kan du använda en kortare form av kommandot genom att lägga till utropstecken efter test och använda "-e" istället för "-d". Detta kommer att returnera "true" eller "false" beroende på om mappen existerar eller inte.

```Bash
if test ! -e /path/to/directory; then
    echo "Mappen existerar inte."
fi
```

## Djupdykning

Det finns några saker att tänka på när du kontrollerar om en mapp existerar. För det första måste du se till att du har rätt rättigheter att läsa innehållet i mappen. Om du försöker kontrollera en mapp som tillhör en annan användare eller med root-rättigheter, kan du använda kommandot "sudo" för att köra kontrollen med rätt behörigheter.

En annan sak att tänka på är att om mappen du kontrollerar innehåller mellanslag eller andra specialtecken, måste du se till att du inkluderar dessa i sökvägen med hjälp av citationstecken.

## Se även

- [Bash-guide från TLDP](https://tldp.org/LDP/abs/html/)
- [Sökvägar och filer i Bash](https://www.maketecheasier.com/understanding-file-paths-in-linux/) 
- [Kom igång med Bash](https://www.shell-tips.com/bash/)