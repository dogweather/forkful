---
title:                "Go: Att Göra En Sträng STOR"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Varför

Att ha möjligheten att göra delar av en sträng stora och andra små kan vara viktigt för både läsbarheten och funktionaliteten hos en Go-applikation. I denna bloggpost kommer vi att titta närmare på hur man kan använda sig av Go för att kapitalisera en sträng.

##Så här gör du

För att kapitalisera en sträng i Go kan du använda funktionen `strings.ToUpper()`. Den tar en sträng som argument och returnerar en kopia av strängen med alla bokstäver stora.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hej världen"
    kapitaliserad := strings.ToUpper(str)

    fmt.Println(kapitaliserad) // Output: HEJ VÄRLDEN
}
```

En annan användbar funktion är `strings.Title()`, som också kapitaliserar strängen men behåller det ursprungliga ordet och ändrar bara den första bokstaven till stor.

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "hej BÄSTA världen"
    kapitaliserad := strings.Title(str)

    fmt.Println(kapitaliserad) // Output: Hej Bästa Världen
}
```

Det är också viktigt att notera att båda dessa funktioner inte ändrar den ursprungliga strängen, utan returnerar en ny kapitaliserad version som måste tilldelas till en variabel för att användas.

##Djupdykning

För mer avancerade behov av att kapitalisera strängar, som att hantera speciella tecken eller hantera flerspråkiga strängar, kan det vara bättre att använda paketet `golang.org/x/text`. Detta paket erbjuder mer avancerade metoder för att hantera olika typer av strängar.

Du kan också använda en loop för att gå igenom en sträng och manuellt ändra bokstäverna till stora.

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    str := "hej världen"
    var kapitaliserad string

    for _, char := range str {
        kapitaliserad += string(unicode.ToUpper(char))
    }

    fmt.Println(kapitaliserad) // Output: HEJ VÄRLDEN
}
```

##Se också

- [Go offciell dokumetation om `strings` paketet](https://golang.org/pkg/strings/)
- [Paketet `golang.org/x/text`](https://golang.org/x/text)
- [Uppgift: Skapa en funktion för att kapitalisera en sträng i Go med hjälp av olika metoder](https://play.golang.org/p/2Ex0wY1daOg)