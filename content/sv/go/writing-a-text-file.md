---
title:    "Go: Att skriva en textfil."
keywords: ["Go"]
---

{{< edit_this_page >}}

##Varför

Att skriva en textfil är en viktig del av att programmera i de flesta språk, inklusive Go. Det är ett enkelt sätt att spara och dela data, och används ofta för att skapa och mata in konfigurationsfiler.

##Hur man gör

Att skriva en textfil i Go är ganska enkelt. Först måste vi skapa en fil att spara vår text i. Det kan vi göra genom att använda funktionen "Create" från "os" paketet. Sen kan vi använda funktionen "WriteString" för att skriva vår text till filen.

```
Go
package main

import (
    "fmt"
    "os"
)

func main() {
    f, err := os.Create("minfil.txt")
    if err != nil {
        fmt.Println(err)
        return
    }
    defer f.Close()
    _, err2 := f.WriteString("Det här är en text som sparas i filen.")
    if err2 != nil {
        fmt.Println(err2)
        return
    }
}
```

Det här enkla exemplet skapar en textfil med namnet "minfil.txt" och sparar texten "Det här är en text som sparas i filen." i filen. Genom att använda "defer" funktionen kan vi se till att filen stängs när programmet är klart, vilket undviker eventuella problem med att läsa eller ändra filen senare.

##Djupdykning

Att skriva till en textfil i Go är ganska enkelt, men det finns mer vi kan göra för att kontrollera hur vår data sparas. Vi kan till exempel använda "bufio" paketet för att skriva vår text med en buffrad skrivare, vilket kan göra det snabbare och mer effektivt.

Vi kan också använda "ioutil" paketet för att läsa eller skriva till en textfil med bara en rad kod, istället för att behöva använda flera funktioner från "os" paketet.

Det finns också många andra aspekter av att skriva textfiler i Go att utforska, som hur man kontrollerar rättigheter och behörigheter till filen, eller hur man lägger till metadata till filen.

##Se även

- [Official GoOS documentation](https://golang.org/pkg/os/)
- [Official Go bufio documentation](https://golang.org/pkg/bufio/)
- [Official Go ioutil documentation](https://golang.org/pkg/io/ioutil/)