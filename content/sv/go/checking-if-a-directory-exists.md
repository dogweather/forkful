---
title:                "Kontrollera om en katalog existerar"
aliases:
- sv/go/checking-if-a-directory-exists.md
date:                  2024-02-03T17:53:23.406072-07:00
model:                 gpt-4-0125-preview
simple_title:         "Kontrollera om en katalog existerar"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Vad och varför?

Att kontrollera om en katalog finns i Go är avgörande för applikationer som interagerar med filsystemet, för att undvika fel när man försöker komma åt eller modifiera kataloger. Denna operation är avgörande för uppgifter som att säkerställa förutsättningar för filoperationer, konfigurationshantering och distribution av programvara som är beroende av specifika katalogstrukturer.

## Hur man gör:

I Go tillhandahåller paketet `os` funktionaliteter för att interagera med operativsystemet, inklusive att kontrollera om en katalog finns. Så här kan du göra det:

```go
package main

import (
    "fmt"
    "os"
)

// isDirExists kontrollerar om en katalog finns
func isDirExists(path string) bool {
    info, err := os.Stat(path)
    if os.IsNotExist(err) {
        return false
    }
    return info.IsDir()
}

func main() {
    dirPath := "/tmp/exampleDir"

    if isDirExists(dirPath) {
        fmt.Printf("Katalog %s finns.\n", dirPath)
    } else {
        fmt.Printf("Katalog %s finns inte.\n", dirPath)
    }
}
```
Exempelutdata:

```
Katalog /tmp/exampleDir finns.
```
eller 

```
Katalog /tmp/exampleDir finns inte.
```

Beroende på om `/tmp/exampleDir` finns.

## Djupdykning

Funktionen `os.Stat` returnerar ett `FileInfo`-gränssnitt och ett fel. Om felet är av typen `os.ErrNotExist`, betyder det att katalogen inte finns. Om det inte finns något fel, kontrollerar vi vidare om vägen verkligen refererar till en katalog genom metodet `IsDir()` från `FileInfo`-gränssnittet.

Denna metod utmärker sig på grund av sin enkelhet och effektivitet, men det är viktigt att notera att att kontrollera en katalogs existens innan man gör operationer som att skapa eller skriva kan leda till tillståndstävlingar i samtidiga miljöer. För många scenarier, speciellt i samtidiga applikationer, kan det vara säkrare att försöka med operationen (t.ex. filskapande) och hantera fel efteråt, snarare än att kontrollera först.

Historiskt sett har denna ansats varit vanlig inom programmering på grund av dess raka logik. Men, utvecklingen av flertrådad och samtidig databehandling kräver en förskjutning mot mer robust felhantering och att undvika förhandskontroller som denna där det är möjligt. Detta minskar inte dess nytta för enklare, enkeltrådade applikationer eller skript där sådana förhållanden är mindre av en oro.
