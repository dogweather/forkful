---
title:    "Go: Konvertera en sträng till versaler"
keywords: ["Go"]
---

{{< edit_this_page >}}

## Varför

Att använda sig av string capitalization är en viktig del av textbehandling i många olika programmeringsspråk, inklusive Go. Genom att förstå hur man korrekt kapitaliserar en sträng kan du förbättra användarupplevelsen och kvaliteten på din kod.

## Så här gör du

För att kapitalisera en sträng i Go kan du använda dig av funktionen "ToTitle" från paketet "strings". Detta kommer att konvertera alla bokstäver till versaler. Vår kod kan se ut så här:

```Go
package main

import "fmt"
import "strings"

func main() {
    str := "hej, jag heter Johan"
    fmt.Println(strings.ToTitle(str))
}
```

Output:

HEJ, JAG HETER JOHAN

Om du vill kapitalisera enbart första bokstaven i en sträng, kan du använda funktionen "Title" istället. Detta kommer också att behålla eventuella redan befintliga versaler. Koden skulle se ut så här:

```Go
package main

import "fmt"
import "strings"

func main() {
    str := "hej, jag heter Johan"
    fmt.Println(strings.Title(str))
}
```

Output:

Hej, Jag Heter Johan

## Djupdykning

Att förstå hur man korrekt kapitaliserar en sträng är viktigt för att undvika fel och göra din kod enklare att läsa och förstå för andra utvecklare. Det finns också olika sätt att kapitalisera en sträng beroende på språkets grammatiska regler. Till exempel har svenska och engelska olika regler för hur ord ska kapitaliseras.

Det är också viktigt att komma ihåg att funktionerna "ToTitle" och "Title" i Go endast fungerar för ASCII-tecken. Detta innebär att om du har specialtecken i din sträng, så kommer de inte att kapitaliseras korrekt. För att lösa detta, kan du använda paketet "unicode" och funktionen "ToTitleSpecial" som tar hänsyn till icke-ASCII tecken.

## Se även

- [Official Go Documentation on Strings](https://golang.org/pkg/strings/)
- [Article on Unicode and Text Processing in Go](https://blog.golang.org/strings)
- [Additional string manipulation packages for Go](https://pkg.go.dev/search?q=strings)