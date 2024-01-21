---
title:                "Hämta en webbsida"
date:                  2024-01-20T17:44:12.782716-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hämta en webbsida"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Nedladdning av en webbsida handlar om att hämta dess innehåll från internet till din dator. Programmerare gör detta för att analysera innehållet, testa tillgänglighet eller samla information.

## Hur man gör:
```go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func main() {
	response, err := http.Get("http://example.com")
	if err != nil {
		fmt.Println("Fel uppstod:", err)
		return
	}
	defer response.Body.Close()

	file, err := os.Create("example.html")
	if err != nil {
		fmt.Println("Fel uppstod vid skapande av fil:", err)
		return
	}
	defer file.Close()

	_, err = io.Copy(file, response.Body)
	if err != nil {
		fmt.Println("Fel uppstod vid skrivning till fil:", err)
		return
	}

	fmt.Println("Webbsida nedladdad som example.html")
}
```
*Sample output:*
```
Webbsida nedladdad som example.html
```

## Fördjupning
Förr användes ofta kommandot `wget` eller `curl` i terminalen för att ladda ner webbsidor. I Go använder vi `net/http`-paketet, vilket ger oss mer kontroll och möjlighet att integrera nedladdningen i större applikationer. Överföringen sker via HTTP (eller HTTPS för säkra anslutningar). Implementationen hanterar låg-nivå detaljer som att upprätta TCP/IP-anslutningar och tolka HTTP-protokollet.

Alternativ för nedladdning inkluderar paket som `colly` för webbskrapning eller `http.Client` för mer anpassade förfrågningar, som hantering av cookies och timeouts.

## Se även
- [Go's `net/http` package documentation](https://pkg.go.dev/net/http)
- [The Go Programming Language Specification](https://go.dev/ref/spec)
- [Using `http.Client` for more advanced HTTP requests](https://pkg.go.dev/net/http#Client)