---
title:                "Sammanslagning av strängar"
html_title:           "C++: Sammanslagning av strängar"
simple_title:         "Sammanslagning av strängar"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/concatenating-strings.md"
---

{{< edit_this_page >}}

# Sätta ihop strängar i Go - En nybörjarguide

## Vad & Varför?
Att sammanfoga strängar, eller string concatenation, innebär att du sätter ihop två eller flera strängar till en enda. Programmerare gör detta för att manipulera textdata på effektiva sätt.

## Så här gör du: 
Låt oss dyka rätt in i enkla koder:

```Go
package main

import "fmt"

func main() {
    str1 := "Hej, "
    str2 := "världen!"
    result := str1 + str2
    fmt.Println(result)
}
```
Utmatning:
```
Hej, världen!
```
Genom att använda '+' operatorn kan vi sätta ihop strängar i Go.

## Djupdykning
Historiskt sett har string concatenationget varit en del av programmeringsspråk sedan deras tidiga dagar. Alternativen till '+' operatorn i Go inkluderar `fmt.Sprintf` och `strings.Join`.

När vi talar om implementeringsdetaljer är det viktigt att nämna att Go hanterar string concatenation på ett mycket effektivt sätt. I många fall optimerar Go kompileraren bort konkatenering helt och hållet och skapar en enda sträng vid kompileringstid.

Här är ett exempel på hur du kan använda `fmt.Sprintf`:

```Go
package main

import "fmt"

func main() {
    str1 := "Hej, "
    str2 := "världen!"
    result := fmt.Sprintf("%s%s", str1, str2)
    fmt.Println(result)
}
```
Och `strings.Join`:

```Go
package main

import (
    "fmt"
    "strings"
)

func main() {
    str1 := "Hej, "
    str2 := "världen!"
    strSlice := []string{str1, str2}
    result := strings.Join(strSlice, "")
    fmt.Println(result)
}
```

## Se också
För mer information rekommenderar vi följande länkar:
1. [Go’s official documentation](https://golang.org/pkg/fmt/#Sprintf): Innehåller djupgående information om `fmt.Sprintf`.
2. [Go’s official documentation](https://golang.org/pkg/strings/#Join): Här hittar du information om `strings.Join`.
3. [General guide on string concatenation](https://en.wikipedia.org/wiki/Concatenation): En allmän guide om konceptet strängkonkatenering.