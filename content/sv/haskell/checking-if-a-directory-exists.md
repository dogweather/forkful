---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

---

# Kolla Om En Katalog Finns Med Haskell

## Vad & Varför?

Att kontrollera om en katalog finns innebär att se till att en specifik sökväg faktiskt pekar på en befintlig katalog. Programmerare gör detta för att förhindra fel som kan uppstå när man försöker manipulera en icke-existerande katalog.

## Hur du gör:

För att kontrollera om en katalog finns i Haskell, kan vi använda `doesDirectoryExist` funktion från `System.Directory` modulen. Se koden nedan:

```Haskell
import System.Directory

main = do
    let dir = "/path/to/directory"
    exists <- doesDirectoryExist dir
    print exists
```
Låt oss köra detta program. Om katalogen finns ger den `True`, om inte ger den `False`.

## Djupdykning

Funktionen `doesDirectoryExist` är en del av `System.Directory`-modulen sedan Haskell Plattformen 2010.2.0.0. Den baseras på Unix-funktionen `stat`, vilket gör den till en snabb och tillförlitlig metod. 

Ett alternativ till `doesDirectoryExist` kan vara att använda `getDirectoryContents` funktion som returnerar en lista med kataloginnehåll, och sen kontrollerar om sökvägen hör till listan. Men detta kan vara mer resurskrävande.

På låg nivå implementeras `doesDirectoryExist` via `getFileInfo` funktion som hämtar information om filsystemet, inklusive filer och kataloger, i ett effektivt och säkert sätt.

## Se även 

För mer information, ta en titt på dessa länkar:

- [`System.Directory` i Haskells basbibliotek](http://hackage.haskell.org/package/directory)
- [Dokumentation av `doesDirectoryExist`](http://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html#v:doesDirectoryExist)