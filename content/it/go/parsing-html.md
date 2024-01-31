---
title:                "Analisi dell'HTML"
date:                  2024-01-20T15:31:53.362727-07:00
simple_title:         "Analisi dell'HTML"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-html.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?

Il parsing di HTML si riferisce all'analisi del codice HTML per estrarne dati specifici o strutturarlo diversamente. I programmatori lo fanno per manipolare o accedere al contenuto delle pagine web, a fini di scraping, analisi o test.

## Come si fa:

In Go, puoi usare il pacchetto `net/html` per effettuare il parsing di HTML. Ecco un esempio semplice:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"strings"
)

func main() {
	const htmlDoc = `
		<!DOCTYPE html>
		<html>
		<head>
			<title>La mia pagina web</title>
		</head>
		<body>
			<p>Ciao, Italia!</p>
		</body>
		</html>
	`

	doc, err := html.Parse(strings.NewReader(htmlDoc))
	if err != nil {
		panic("Errore nel parsing HTML: " + err.Error())
	}

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "p" {
			fmt.Println(n.FirstChild.Data)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}

	f(doc)
}
```

Output:

```
Ciao, Italia!
```

## Approfondimenti:

Il parsing di HTML in Go risale ai primi giorni del linguaggio. Una rappresentazione ad albero del documento HTML viene costruita durante il parsing, consentendo di manipolare o estrarre le parti desiderate.

Altre opzioni includono `goquery` per un'esperienza simile a jQuery o `colly` per lo scraping web. Dettagli implementativi: `net/html` fa uso di tokenizer e parser, che suddividono il testo in token e poi costruiscono l'albero del DOM (Document Object Model).

## Vedi anche:

- Documentazione `net/html`: https://pkg.go.dev/golang.org/x/net/html
- `goquery` per Go: https://github.com/PuerkitoBio/goquery
- `colly`, il framework scraping per Go: http://go-colly.org/
