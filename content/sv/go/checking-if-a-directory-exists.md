---
title:    "Go: Kontrollera om en mapp finns"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/go/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Varför 
Att kontrollera om en mapp existerar är en viktig del av filhanteringen när man programmerar i Go. Det ger en möjlighet att hantera och navigera bland filer och mappar på ett effektivt sätt. 

## Hur man gör 
Det enklaste sättet att kontrollera om en mapp existerar är att använda Go's inbyggda "os" paket. För att göra detta behöver du bara importera paketet och sedan använda funktionen "Stat" för att söka efter mappen. Om mappen existerar returneras en nil error, annars returneras ett felmeddelande. Nedan finns ett kodexempel på hur detta kan göras: 

```Go
import (
    "fmt"
    "os"
)

func main() {
    if _, err := os.Stat("/path/to/directory"); err == nil {
        fmt.Println("Mappen existerar.")
    } else if os.IsNotExist(err) {
        fmt.Println("Mappen existerar inte.")
    } else {
        fmt.Println("Annat fel uppstod.")
    }
}
```

När koden körs och mappen existerar kommer utskriften att vara "Mappen existerar." Om mappen inte existerar kommer utskrift att vara "Mappen existerar inte." Om det uppstår något annat fel kommer felmeddelandet att skrivas ut. 

## Djupdykning 
För en mer robust kontroll av mappen kan man använda sig av "os.FileInfo" objektet som returneras av funktionen "Stat". Detta objekt innehåller mer information om mappen som till exempel storlek och ändringsdatum. Om man bara är intresserad av att kontrollera om mappen är en mapp (och inte en fil) kan man använda funktionen "IsDir" på "FileInfo" objektet. Här är ett exempel på hur man kan göra det: 

```Go
import (
    "fmt"
    "os"
)

func main() {
    if fileInfo, err := os.Stat("/path/to/directory"); err == nil {
        if fileInfo.IsDir() {
            fmt.Println("Mappen är en mapp.")
        } else {
            fmt.Println("Mappen är en fil.")
        }
    } else if os.IsNotExist(err) {
        fmt.Println("Mappen existerar inte.")
    } else {
        fmt.Println("Annat fel uppstod.")
    }
}
```

## Se även
- [Go os paket dokumentation](https://golang.org/pkg/os/)
- [Go filepath paket dokumentation](https://golang.org/pkg/path/filepath/)
- [Go's "if err != nil" idiom](https://blog.golang.org/error-handling-and-go)