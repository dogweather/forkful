---
title:                "Kontrollera om en katalog finns"
html_title:           "Bash: Kontrollera om en katalog finns"
simple_title:         "Kontrollera om en katalog finns"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Vad & Varför?

En grundläggande uppgift i Go programmering är att kontrollera om en mapp existerar. Detta är viktigt för att förhindra potentiella fel som kan uppstå då vi försöker att läsa, skriva eller ändra en mapp som inte finns.

## Hur man gör:

För att kontrollera om en mapp existerar kan vi använda os paketets funktion `os.Stat()`. Här är ett exempel på hur man använder den:

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    _, err := os.Stat("/path/to/directory")
    if os.IsNotExist(err) {
        fmt.Println("Mappen finns inte!")
    } else {
        fmt.Println("Mappen finns!")
    }
}
```

Om mappen inte finns kommer detta program att skriva ut `Mappen finns inte!`. Om mappen finns kommer det att skriva ut `Mappen finns!`.

## Djupdykning:

`os.Stat()` funktionen i Go har använts sedan de tidiga versionerna av Go och är fortfarande det mest rekommenderade sättet att kontrollera om en fil eller mapp existerar. Alternativt kan man använda `os.IsExist(err)` istället för `os.IsNotExist(err)` för att hantera olika feltyper på ett effektivare sätt.

Notera att vi använder ett understreck `_` för att ignorera det första returnerade värdet från `os.Stat()`. I Go är det standard att ignorera värden vi inte använder genom att tilldela dem till `_`.

## Se också:

Mer information om att hantera filer och mappar i Go kan du hitta på följande platser:

- Go Dokumentation: [os paket](https://golang.org/pkg/os/)
- Go Blog: [Go Filesystem](https://blog.golang.org/io2013-talk-file-system)