---
title:                "Go: Sökning och ersättning av text"
programming_language: "Go"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför

Att söka och ersätta text är en vanlig uppgift inom programmering, särskilt när man arbetar med textfiler eller strängar. Genom att kunna effektivt söka och ersätta text kan du spara tid och undvika manuella fel.

## Så här gör du

Det finns flera sätt att söka och ersätta text i Go. En av de enklaste är att använda funktionen "ReplaceAll" från standardpaketet "strings". Här är ett exempel på hur du kan använda det:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "Hej världen"
    fmt.Println(str) // Skriver ut "Hej världen"

    nyStr := strings.ReplaceAll(str, "Hej", "Hallå")
    fmt.Println(nyStr) // Skriver ut "Hallå världen"
}
```

I detta exempel använder vi "strings.ReplaceAll" för att ersätta alla förekomster av "Hej" med "Hallå" i strängen "Hej världen".

Du kan också använda "Replace" för att bara ersätta den första förekomsten av en matchning. Det finns också mer avancerade mönstermatchningsfunktioner som "Regexp" från paketet "regexp".

## Djupdykning

Att förstå hur sök- och ersättningsprocessen fungerar under ytan kan hjälpa dig att bli mer effektiv i din kodning. När det kommer till textmatching finns det två vanliga algoritmer som används: "Boyer-Moore" och "Knuth-Morris-Pratt".

Boyer-Moore-algoritmen fokuserar på att flytta sig i texten så lite som möjligt när den letar efter en matchning. Den börjar från höger sida av söksträngen och arbetar sig tillbaka mot vänster för att hoppa över delar av texten som inte kan matcha. Knuth-Morris-Pratt-algoritmen använder en liknande strategi, men lägger även till ytterligare datastrukturer för att undvika att jämföra tecken som redan har matchats.

Du kan läsa mer om dessa algoritmer och hur de implementeras i Go på dessa länkar:

- [Boyer-Moore-algoritmen](https://en.wikipedia.org/wiki/Boyer–Moore_string_search_algorithm)
- [Knuth-Morris-Pratt-algoritmen](https://en.wikipedia.org/wiki/Knuth–Morris–Pratt_algorithm)

## Se också

- [Go:s stränghanteringspaket](https://golang.org/pkg/strings/)
- [Tutorial: Sök och ersätt i Go](https://gobyexample.com/string-functions)