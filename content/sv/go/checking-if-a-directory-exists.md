---
title:                "Go: Kontrollera om en mapp finns"
programming_language: "Go"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför

Att kontrollera om en mapp finns kan vara en viktig del av en Go-programmerares arbetsflöde. Det kan hjälpa till att säkerställa att nödvändiga filer och mappar finns på plats innan programmet börjar köra.

## Hur man gör det

Att kontrollera om en mapp finns är enkelt med hjälp av Go:s `os` paket. Här är ett exempel på hur man kan göra det:

```Go
package main
import (
  "fmt"
  "os"
)
func main() {
  folder := "/Users/User/Documents"
  if _, err := os.Stat(folder); !os.IsNotExist(err) {
    fmt.Printf("Mappen %s finns redan. \n", folder)
  } else {
    fmt.Println("Mappen finns inte.")
  }
}
```
Resultatet av detta program kommer att vara antingen "Mappen finns inte." eller "Mappen /Users/User/Documents finns redan." beroende på om mappen finns eller inte.

## Deep Dive

För att förstå hur denna kod fungerar kan det vara bra att veta vad varje steg gör. Först importeras paketen `fmt` och `os` som behövs för att skriva ut information och använda funktionerna i `os` paketet.

Nästa steg är att ange sökvägen till mappen som ska kontrolleras i variabeln `folder`. Sedan används `os.Stat()` funktionen för att hämta information om filen eller mappen som specificerats av sökvägen i `folder` variabeln. Om mappen inte finns kommer `os.Stat()` att returnera ett fel. I detta fall kan vi använda `os.IsNotExist()` funktionen för att avgöra om felet betyder att mappen inte finns eller om det är ett annat fel.

Om `os.IsNotExist()` returnerar `true`, med andra ord om mappen inte finns, kommer vi att skriva ut "Mappen finns inte." med hjälp av `fmt.Println()` funktionen. Om mappen finns kommer `os.IsNotExist()` att returnera `false` och `else` satsen kommer att köra, där vi skriver ut att mappen redan finns.

## Se även

- [Go's os-paket](https://golang.org/pkg/os/)
- [How to check if a file or directory exists in Go](https://www.golangprograms.com/check-file-or-folder-exists-or-not.html)
- [Using the Go File System to Check for Directory Existence](https://www.digitalocean.com/community/tutorials/using-the-go-file-system-to-check-for-directory-existence)
- [Kontrollera om en mapp finns i Golang](https://daily.dev/blog/how-to-check-if-a-directory-exists-in-golang)