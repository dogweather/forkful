---
title:                "Interpolera en sträng"
date:                  2024-01-20T17:50:45.986434-07:00
model:                 gpt-4-1106-preview
simple_title:         "Interpolera en sträng"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Stränginterpolering innebär att du blandar variabler och konstanter i en sträng. Programmerare gör detta för att skapa dynamiska meddelanden eller bearbeta information utan att behöva slå ihop strängar manuellt.

## Hur man gör:
```Go
package main

import "fmt"

func main() {
    name := "Ingrid"
    age := 28
    greeting := fmt.Sprintf("Hej, mitt namn är %s och jag är %d år gammal.", name, age)
    
    fmt.Println(greeting) // Output: Hej, mitt namn är Ingrid och jag är 28 år gammal.
}
```

## Djupdykning
I äldre programmeringsspråk som C förlitade man på funktioner som `sprintf` för interpolering. Go erbjuder `fmt.Sprintf`, som använder verb som `%s` för strängar och `%d` för heltal. Sedan Go 1.18, kan man även använda `strings.Builder` för att effektivisera skapandet av stora strängar, vilket är nyttigt när prestanda räknas.

Alternativ till `fmt.Sprintf` inkluderar `+` eller `fmt` paketets `Sprint` och `Sprintln` funktioner för enklare sammanfogning. Interpolering hjälper till att hålla koden ren och med lättare underhåll än att manuellt slå ihop många strängar.

## Se även
- Go-dokumentation för `fmt` paketet: https://pkg.go.dev/fmt
- Go by Example om strängformatering: https://gobyexample.com/string-formatting
- Go bloggpost om stränger: https://blog.golang.org/strings
