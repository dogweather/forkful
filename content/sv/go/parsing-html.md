---
title:                "Analysera html"
html_title:           "Arduino: Analysera html"
simple_title:         "Analysera html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
HTML-parsing innebär att förvandla en sträng med HTML-kod till ett användbart objekt i ditt program. Genom att göra det kan programmerare manipulera, undersöka och extrahera information från webbsidor.

## Hur gör man:
Här är ett enkelt exempel på hur man kan använda `golang.org/x/net/html` paketet för att parse HTML.

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	s := "<html><body><p>Hej världen!</p></body></html>"
	doc, _ := html.Parse(strings.NewReader(s))
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.TextNode {
			fmt.Println(n.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```

När du kör koden, kommer du att se "Hej världen!" skriven i din konsol.

## Djupdykning
HTML-parsing har funnits sedan HTML först skapades. Det finns många olika sätt att lösa problemet, men `golang.org/x/net/html` paketet i Go är en favorit på grund av dess enkelhet och effektivitet.

Ett annat alternativ är att använda `net/html/charset` paketet som är användbart om du behöver stöd för olika teckenuppsättningar.

När det gäller implementation detaljer, utför Go-parsern en viss normalisering på HTML-koden, såsom att konvertera alla taggar och attribut till små bokstäver.

## Se också
Här är några bra källor för mer information och verktyg:

- Mer om HTML parsing: https://www.golangprograms.com/golang-parsing-html.html
- net/html paketet: https://godoc.org/golang.org/x/net/html
- UTF-8 och Go: https://blog.golang.org/strings
- Ett alternativ, `net/html/charset` paketet: https://godoc.org/golang.org/x/net/html/charset