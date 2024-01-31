---
title:                "Przetwarzanie HTML"
date:                  2024-01-20T15:31:53.098220-07:00
simple_title:         "Przetwarzanie HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?
Parsing HTML umożliwia ekstrakcję danych z dokumentów HTML. Programiści używają tego do analizy struktur stron internetowych, autoryzacji treści, czy do scrapingu danych.

## Jak to zrobić:
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	doc, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Println(a.Val)
					break
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}

	f(doc)
}
```
Sample output:
```
http://www.iana.org/domains/example
```

## Wnikliwe spojrzenie:
HTML, od lat 90. XX wieku, jest standardem webowym. Parsing HTML zwykle używa DOM (Document Object Model). Alternatywy to regexy (ale uwaga: [nie są do tego zalecane](https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags)) czy biblioteki takie jak BeautifulSoup w Pythonie. W Go, `golang.org/x/net/html` to oficjalne narzędzie do parsingu, oparte o tokenizację, zapewniające dostęp do elementów DOM bez uruchamiania JS czy stosowania zewnętrznych usług.

## Zobacz także:
- Dokumentacja `net/html`: https://pkg.go.dev/golang.org/x/net/html
- Oficjalny tutorial Go na temat scrapingu webowego: https://golang.org/doc/articles/wiki/
- Artykuł o problemach z używaniem regexów do parsingu HTML: https://blog.codinghorror.com/parsing-html-the-cthulhu-way/
