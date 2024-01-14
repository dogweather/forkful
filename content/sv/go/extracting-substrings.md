---
title:    "Go: Extrahering av delsträngar"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför
I Go-språket används substrängar ofta för att manipulera textdata. Det kan vara användbart för att extrahera specifika delar av en textsträng eller för att hantera användarinputs. I denna bloggpost kommer vi att utforska hur man använder substrängar i Go och varför det kan vara användbart.

## Så här gör du
För att extrahera en substräng från en textsträng i Go kan du använda funktionen "strings.Index" för att hitta positionen för det första tecknet i substrängen. Sedan kan du använda funktionen "strings.Substr" för att extrahera själva substrängen. Här är ett exempel på hur du kan göra detta:

```Go 
package main

import (
  "fmt"
  "strings"
)

func main() {
  string := "Hej, det här är en textsträng"
  index := strings.Index(string, "är") + 1 // +1 för att inkludera det första tecknet i sökningen
  substring := string[index:]
  fmt.Println(substring)
}
```

Detta kommer att skriva ut "är en textsträng" eftersom substrängen som vi extraherade börjar på positionen för det första tecknet efter "är".

## Djupdykning
En intressant funktion i Go-språket är att substrängar är en deltyp av en huvudsträng och delar samma underliggande array. Det innebär att om du ändrar en substräng, kommer även den ursprungliga strängen att ändras. Detta kan ibland orsaka problem, så det är viktigt att vara medveten om detta. En annan viktig aspekt är att substrängar är immutabla, vilket betyder att de inte kan ändras direkt. Om du behöver ändra en substräng måste du tilldela den till en ny variabel efter att ha använt "strings.Substr".

## Se även
Här är några andra resurser som kan vara användbara för att förbättra din kunskap om substrängar i Go:
- https://gobyexample.com/substring
- https://www.golang-book.com/books/intro/7
- https://tour.golang.org/basics/5