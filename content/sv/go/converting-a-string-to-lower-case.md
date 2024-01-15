---
title:                "Omvandla en sträng till gemener"
html_title:           "Go: Omvandla en sträng till gemener"
simple_title:         "Omvandla en sträng till gemener"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Varför

Det finns många användbara fall där man behöver konvertera en sträng till gemener (lower case) i sitt Go-program. Till exempel kan det vara för formatering av data, sökningar eller jämförelser av strängar.

## Så här gör du

För att konvertera en sträng till gemener i Go använder vi funktionen `tolower` från paketet `strings`. Här är ett enkelt exempel:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str := "HELLO WORLD"
    lowerStr := strings.ToLower(str)
    fmt.Println(lowerStr)
}
```

Detta kommer att skriva ut "hello world" i terminalen. Notera att funktionen modifierar inte ursprungliga strängen utan returnerar istället en ny sträng.

Om du vill använda en annan enkelt sätt att konvertera en enskild bokstav till gemener, kan du använda `byte`-metoden `ToLower`. Här är ett exempel:

```Go
package main

import "fmt"

func main() {
    ch := 'A'
    lowerCh := byte(ch) + 'a' - 'A'
    fmt.Println(string(lowerCh))
}
```

I detta exempel konverterar vi bokstaven "A" till "a" genom att addera skillnaden mellan "a" och "A". Denna metod fungerar endast med bokstäver från A-Z.

## Djupdykning

För det mesta är det enkelt att konvertera en sträng till gemener i Go. Men vad händer om din sträng innehåller icke-engelska bokstäver? Till exempel "Ä" eller "Ö"? I standardbiblioteket för Go finns det inte ett enkelt sätt att konvertera dessa bokstäver till gemener.

Istället måste vi använda paketet `unicode` för att konvertera dessa bokstäver. Här är ett exempel:

```Go
package main

import (
    "fmt"
    "unicode"
)

func main() {
    str := "Äpple"
    var lowerStr string
    for _, r := range str {
        lowerStr += string(unicode.ToLower(r))
    }
    fmt.Println(lowerStr)
}
```

I detta exempel loopar vi genom varje tecken i strängen och använder `unicode.ToLower` för att konvertera varje tecken till gemener. Slutresultatet kommer att bli "äpple". Detta kan vara lite mer krångligt, men det fungerar även för icke-engelska bokstäver.

## Se även

 * [Go Strings Package](https://golang.org/pkg/strings/)
 * [Go Unicode Package](https://golang.org/pkg/unicode/)