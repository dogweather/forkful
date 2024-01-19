---
title:                "Scaricare una pagina web"
html_title:           "C++: Scaricare una pagina web"
simple_title:         "Scaricare una pagina web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Scaricare una pagina web con Go

## Cos'è & Perché?

Scaricare una pagina web significa acquisire il codice HTML di una pagina web. I programmatori lo fanno per analizzare, manipolare o presentare i contenuti web in modi utili ed efficienti.

## Come fare:

Scarica una pagina web con Go in maniera semplice. Qui c'è un esempio di codice:

```Go
package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
    defer resp.Body.Close()

    out, err := os.Create("output.html")
    if err != nil {
        panic(err)
    }
    defer out.Close()

    _, err = io.Copy(out, resp.Body)
    if err != nil {
        panic(err)
    }
}
```

L'esecuzione di questo script scaricherà il codice HTML di 'http://example.com' e lo salverà come 'output.html' nel tuo percorso di esecuzione corrente.

## Approfondimento

Historicamente, i programmatori usavano programmi come 'wget' o 'curl' per scaricare pagine web. Questi strumenti erano adeguati per scaricare singole pagine web, ma non erano pratici quando era necessario programmative scaricare o analizzare molte pagine.

Le alternative a Go per scaricare pagine web includono Python con le librerie come 'requests' o 'urllib', o Node.js con 'axios' o 'request'.

Go offre una serie di vantaggi. È efficiente, veloce, e grazie alla natura concorrente di Go, è facile scaricare pagine web in parallelo. Inoltre, Go standard library ha tutto il necessario per scaricare pagine web senza dover dipendere da librerie esterne.

## Vedere Anche

Puoi trovare ulteriori informazioni sui seguenti link:

1. Documentazione Go net/http package: [https://golang.org/pkg/net/http](https://golang.org/pkg/net/http/)
2. Un tutorial Go su come scaricare file: [https://golangcode.com/download-a-file-from-a-url/](https://golangcode.com/download-a-file-from-a-url/)
3. Un articolo su come usare Go per il web scraping: [https://edmundmartin.com/writing-a-web-crawler-in-golang/](https://edmundmartin.com/writing-a-web-crawler-in-golang/)