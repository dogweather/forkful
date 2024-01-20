---
title:                "Analisi sintattica dell'HTML"
html_title:           "C++: Analisi sintattica dell'HTML"
simple_title:         "Analisi sintattica dell'HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Il parsing HTML comporta l'analisi del markup di una pagina web in un formato strutturato. I programmatori lo fanno per estrarre dati specifici, manipolare contenuti o interagire con il web a un livello più profondo.

## Come si fa:
Go offre il pacchetto `net/html` per eseguire il parsing HTML. Ecco un esempio:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	s := "<html><head><title>Hi</title></head><body>Hello, Go!</body></html>"
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
E il risultato sarà:
```
Hi
Hello, Go!
```

## Approfondimento
Parsing HTML ha le sue radici storiche nel primo web, dove i programmatori richiedevano dati da pagine HTML. Go offre una soluzione elegante con il pacchetto `net/html`, ma ci sono alternative come il pacchetto `goquery`. La decisione su quale utilizzare dipende dalle esigenze specifiche del progetto. Ad esempio, `net/html` è più leggero e adatto per parsing semplice, ma `goquery` offre un'interfaccia jQuery-like che facilita l'iterazione e la manipolazione degli elementi DOM.

## See Also
1. Documentazione del pacchetto `net/html`: https://pkg.go.dev/golang.org/x/net/html
2. Documentazione del pacchetto `goquery`: https://pkg.go.dev/github.com/PuerkitoBio/goquery
3. Esempi di parsing HTML in Go: https://socketloop.com/tutorials/golang-parse-html-example