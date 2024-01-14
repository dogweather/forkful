---
title:                "Go: Söka och ersätta text"
simple_title:         "Söka och ersätta text"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Varför
Att söka och ersätta text är en vanlig process inom programmering som kan effektivisera och underlätta arbetsflödet. I Go-språket finns det många inbyggda funktioner som gör det enkelt att utföra sökningar och ersättningar i text.

## Hur man gör
För att söka och ersätta text i Go använder man sig av funktionen "strings.Replace()". Denna funktion tar in tre argument: strängen som ska sökas igenom, strängen som ska ersättas och det antal gånger som ersättningen ska göras. Nedan finns ett exempel på hur man kan använda funktionen:

```Go
package main

import (
	"fmt"
	"strings"
)

func main() {
	originalStr := "Välkommen till Golang!"
	ersattStr := strings.Replace(originalStr, "Golang", "Go", 1)
	fmt.Println(ersattStr)
}
```

Output:
```
Välkommen till Go!
```

Detta exempel visar hur man enkelt kan ersätta en del av en sträng med en annan del.

## Djupdykning
För den som vill veta mer om hur sökningar och ersättningar fungerar i Go-språket finns det flera olika metoder och tekniker som man kan använda sig av. En vanlig teknik är att använda reguljära uttryck för att söka efter mönster i en sträng. Go har inbyggda paket för reguljära uttryck, såsom "regexp", som gör det enkelt att söka och ersätta text baserat på ett givet mönster.

## Se även
- [Go-dokumentationen för strings.Replace()](https://golang.org/pkg/strings/#Replace)
- [En guide till reguljära uttryck i Go](https://www.digitalocean.com/community/tutorials/how-to-use-regular-expressions-in-go)
- [Go:s inbyggda paket för reguljära uttryck](https://golang.org/pkg/regexp/)