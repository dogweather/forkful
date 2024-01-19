---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att kontrollera om en katalog finns är en aktion som bekräftar om en angiven katalog faktiskt finns i ditt filsystem. Programmerare gör detta för att undvika fel som kan uppstå om de försöker manipulera en icke-existerande katalog.

## Så här gör du:
Låt oss ta en titt på hur du kan kontrollera om en katalog existerar i Fish Shell.

```Fish Shell
set directory "/min/väg/till/katalog"

if test -d $directory
    echo "Katalogen finns"
else
    echo "Katalogen finns inte"
end
```
Om "katalogen finns", visas det meddelandet. Om katalogen inte finns, visas "Katalogen finns inte".

## Djupdykning
Konceptet att kontrollera om en katalog finns går tillbaka till de tidigaste dagarna för programmering. Det är ett grundläggande verktyg i programmerarens verktygslåda och något som du kommer att stöta på ganska ofta om du jobbar med filsystem.

Ett alternativ till ovanstående är att använda en "try/catch"-block. Men, `test -d` är mer konkret och tillförlitligt.

För att implementera detta, använder `test -d` kommandot underliggande systemanrop för att fråga filsystemet om katalogens existens. Detta är extremt snabbt och effektivt, men det är naturligtvis beroende av din åtkomst till filsystemet.

## Se också:
- Fish Shell dokumentation om 'test': [https://fishshell.com/docs/current/cmds/test.html](https://fishshell.com/docs/current/cmds/test.html)
- Ytterligare läsning om filsystem: [https://en.wikipedia.org/wiki/File_system](https://en.wikipedia.org/wiki/File_system)