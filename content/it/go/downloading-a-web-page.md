---
title:                "Scaricando una pagina web"
html_title:           "Go: Scaricando una pagina web"
simple_title:         "Scaricando una pagina web"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Perché

Scaricare una pagina web può essere utile per ottenere informazioni o dati specifici contenuti nella pagina, o semplicemente per salvare una copia della pagina per poterla visualizzare offline.

## Come fare

Per scaricare una pagina web in Go, possiamo utilizzare la libreria standard `net/http` e la funzione `HttpGet` per effettuare una richiesta HTTP GET al server del sito web desiderato.

```Go
package main
import (
    "fmt"
    "io/ioutil"
    "net/http"
)

func main() {
    // Effettua una richiesta GET al sito web
    response, err := http.Get("https://www.example.com")
    if err != nil {
        panic(err)
    }
    defer response.Body.Close()

    // Legge il contenuto della risposta
    body, err := ioutil.ReadAll(response.Body)
    if err != nil {
        panic(err)
    }

    // Stampa il contenuto della pagina web
    fmt.Println(string(body))
}
```

L'output del codice precedente sarà il codice HTML della pagina web. Possiamo anche specificare un file di destinazione per salvare il contenuto invece di stamparlo a schermo:

```Go
// Scrittura su file
err = ioutil.WriteFile("pagina.html", body, 0644)
if err != nil {
    panic(err)
}
```

## Deep Dive

Oltre alla funzione `HttpGet` della libreria standard, esistono altre opzioni per scaricare una pagina web in Go. Ad esempio, la libreria `goquery` permette di analizzare il codice HTML della pagina e ottenere specifici elementi o dati.

Possiamo anche utilizzare altri metodi HTTP oltre al `GET`, come ad esempio `POST` o `PUT`, e specificare parametri o header personalizzati nella nostra richiesta.

## Vedi anche

- [Documentazione ufficiale di Go sulla libreria `net/http`](https://golang.org/pkg/net/http/)
- [Libreria `goquery`](https://github.com/PuerkitoBio/goquery) per l'analisi del codice HTML
- [Tutorial su come scaricare una pagina web in Go](https://tutorialedge.net/golang/making-http-requests-in-golang/) su Tutorialedge.net