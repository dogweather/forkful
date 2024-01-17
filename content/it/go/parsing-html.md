---
title:                "Analisi di html."
html_title:           "Go: Analisi di html."
simple_title:         "Analisi di html."
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-html.md"
---

{{< edit_this_page >}}

"#

## Che cos'è e perché?
Il parsing HTML è il processo di analisi e comprensione del codice HTML di una pagina web. I programmatori lo fanno per estrarre informazioni specifiche dalla pagina, come testo, immagini o link, e utilizzarle per scopi vari come il data scraping o la creazione di applicazioni web personalizzate.

## Come fare:
Di seguito sono riportati alcuni esempi di codice in Go per il parsing HTML. Proviamoli direttamente nella tua console Go e osserva l'output.

```Go
package main

import (
	"fmt"
	"net/http"
	"io/ioutil"
	"golang.org/x/net/html"
)

func main() {
	resp, _ := http.Get("https://www.example.com")

	byteValue, _ := ioutil.ReadAll(resp.Body)

	doc, _ := html.Parse(strings.NewReader(string(byteValue)))

	// 1. Estrazione di testo:
	var extractText func(*html.Node) string
	extractText = func(n *html.Node) string {
		if n.Type == html.TextNode {
			return n.Data
		}
		text := ""
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			text += extractText(c) + " "
		}
		return strings.Join(strings.Fields(text), " ")
	}
	fmt.Println(extractText(doc))

	// 2. Estrazione di immagini:
	var extractImages func(*html.Node)
	extractImages = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "img" {
			for _, a := range n.Attr {
				if a.Key == "src" {
					fmt.Println(a.Val)
					break
				}
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			extractImages(c)
		}
	}
	extractImages(doc)
}
```

## Approfondimento:
Il parsing HTML è diventato un'operazione comune grazie allo sviluppo del web e alla creazione di linguaggi di markup come HTML. Esistono anche altri metodi per estrarre dati da una pagina web, come l'utilizzo di API o l'utilizzo di librerie specifiche per il data scraping. 
Nel processo di parsing HTML, esistono diverse metodologie per l'analisi del codice, come l'algoritmo di parsing a albero o il parsing basato su espressioni regolari. Inoltre, è importante considerare la compatibilità con le diverse versioni di HTML.

## Vedi anche:
- [Documentazione ufficiale di Go per il parsing HTML](https://golang.org/pkg/html/)
- [Esempi di parsing HTML in Go](https://golangcode.com/how-to-parse-html-in-go/)
- [Altri metodi per l'estrazione di dati da una pagina web](https://www.scrapingbee.com/blog/web-scraping-vs-apis-which-is-better/)