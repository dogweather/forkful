---
title:                "Generera slumpmässiga nummer"
html_title:           "Arduino: Generera slumpmässiga nummer"
simple_title:         "Generera slumpmässiga nummer"
programming_language: "Go"
category:             "Go"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/generating-random-numbers.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Generering av slumpmässiga nummer är när datorprogran skapar nummer på ett oplanerat sätt. Programmerare gör detta för att simulera en händelse med ovisst resultat, som simulerar en tärningkast, eller för att generera unika identifierare.

## Så här gör du:
Här är ett exempel på hur man skapar ett slumpmässigt heltal i Go:

```Go
package main
import (
	"fmt"
	"math/rand"
	"time"
)
func main() {
	rand.Seed(time.Now().UnixNano())
	randomNumber := rand.Intn(100)
	fmt.Println(randomNumber)
}
```
När du kör det här programmet kan din utmatning se ut så här:

```Go
42
```
Remember, resultatet varierar varje gång du kör programmet eftersom det är slumpmässigt.

## Djupdykning
Historiskt sett började generering av slumpmässiga nummer inom datorer med metoder som strålning eller termiskt brus innan programmet innehöll metoder. Alternativ inkluderar krypto-säkra generatorer, men dessa är mer komplexa. I Go kan du använda paketet math/rand, vilket är snabbt och enkelt att använda. Men det bör noteras att Go's standardgenerator inte är krypto-säkert, så den bör inte användas för kryptografiska syften.

## Se även
Relaterade ämnen inkluderar hur man genererar slumpmässiga nummer inom specifika intervall, eller hur man genererar krypto-säkra slumpmässiga nummer. Här är några länkar till Go-dokument om de här ämnena:
- [Go math/rand paket dokumentation](https://golang.org/pkg/math/rand/)
- [Go crypto/rand paket dokumentation](https://golang.org/pkg/crypto/rand/)