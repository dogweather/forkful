---
title:                "Hitta längden på en sträng"
html_title:           "Arduino: Hitta längden på en sträng"
simple_title:         "Hitta längden på en sträng"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att hitta längden på en sträng innebär att ta reda på antalet tecken den innehåller. Det används ofta i programmering för att kontrollera strängvärden, iterera över tecken och mycket annat.

## Hur görs det?
I Go kan du använda den inbyggda `len( )` funktionen för att hitta längden på en sträng. Här är ett exempel:

```Go
package main
  
import "fmt"
  
func main() {
    str := "Hej Sverige"
    fmt.Println(len(str))
}
```
När du kör det här programmet ska det skriva ut "11", som representerar antalet tecken i strängen "Hej Sverige".

## Fördjupning
Hitta strängens längd är en grundläggande operation som går tillbaka till de tidiga dagarna av programmering. I tidigare programmeringsspråk, som C, måste programmerare iterera genom en sträng tills de nådde ett null-tecken. 

Go tar en annorlunda väg, varje sträng är ett par: ett pekare till en array av bytes och en längd. Därför är komplexiteten för len-funktionen O(1).

Alternativt kan du använda `range` och en lök till att iterera över strängen, räkna upp ett värde för varje tecken. Denna metod tar mer kod och är mindre effektiv, men kan vara användbar om du också behöver bearbeta varje tecken.

```Go
package main

import "fmt"
  
func main() {
    str := "Hej Sverige"
    count := 0
    for range str {
        count++
    }
    fmt.Println(count)
}
```
Detta program skulle också resultera i utdata "11".

## Se också
Huvuddokumentation för `len`: https://golang.org/pkg/builtin/#len 

Mer om Go strängar och karaktärer: https://go.dev/blog/strings
Strängteorier i Go: https://blog.golang.org/strings