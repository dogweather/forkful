---
title:                "Analisi di html"
html_title:           "Go: Analisi di html"
simple_title:         "Analisi di html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/parsing-html.md"
---

{{< edit_this_page >}}

## Perché

Se stai sviluppando un sito web o un'applicazione che richiede di estrarre informazioni da una pagina HTML, molto probabilmente dovrai utilizzare il parsing HTML. In breve, il parsing HTML ti permette di leggere e analizzare il contenuto di una pagina HTML in modo strutturato, rendendo più facile l'estrazione di dati specifici.

## Come fare

Per eseguire il parsing di una pagina HTML in Go, puoi utilizzare la libreria "html" incorporata nel linguaggio. Ecco un esempio di codice per estrarre il titolo di una pagina HTML e stamparlo a schermo:

```Go
package main

import (
	"fmt"
	"net/http"
	"golang.org/x/net/html"
)

func main() {
	resp, err := http.Get("https://www.example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	doc, err := html.Parse(resp.Body)
	if err != nil {
		panic(err)
	}

	var title string
	var findTitle func(*html.Node)
	findTitle = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "title" {
			title = n.FirstChild.Data
			return
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			findTitle(c)
		}
	}

	findTitle(doc)
	fmt.Println(title) // Stampa il titolo della pagina
}
```

L'esempio utilizza la funzione http.Get() per ottenere il contenuto della pagina e poi lo passa alla funzione html.Parse() per analizzarlo. Utilizzando la funzione ricorsiva findTitle(), vengono cercati tutti i nodi HTML con il tag "title" e il loro contenuto viene assegnato alla variabile "title". Infine, il titolo viene stampato a schermo.

## Approfondimento

Oltre al semplice parsing di tag HTML, la libreria "html" di Go permette anche di manipolare i nodi della pagina e ottenere attributi specifici. Inoltre, puoi combinare il parsing HTML con la libreria "net/http" per scaricare in modo automatico il contenuto di una pagina web.

Vale la pena notare che la libreria "html" non supporta il parsing di pagine HTML incomplete o non valide. Pertanto, è importante assicurarsi che la pagina sia correttamente formattata prima di tentare il parsing.

## Vedi anche

- [Documentazione ufficiale su Go per il parsing HTML](https://golang.org/pkg/html/)
- [Tutorial di parsing HTML in Go su Medium](https://medium.com/@zeebo/parsing-html-in-golang-5581e93711cf)
- [Esempio di parsing HTML in Go su GitHub](https://github.com/zeebo/html-example)