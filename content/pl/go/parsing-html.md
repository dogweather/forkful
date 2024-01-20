---
title:                "Analiza składniowa HTML"
html_title:           "Gleam: Analiza składniowa HTML"
simple_title:         "Analiza składniowa HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/parsing-html.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Analiza składni (parsing) HTML polega na przetworzeniu struktury dokumentu HTML na dane, które są zrozumiałe i użyteczne dla programu. Programiści robią to, aby uzyskać wyższy poziom kontroli i zrozumienia struktury i zawartości stron www, pozyskiwać z nich dane lub manipulować nimi.

## Jak to zrobić:

For the sake of translation, the comments in the code below are in Polish for your understanding.

```Go
package main

import (
    "fmt"
    "golang.org/x/net/html"
    "net/http"
    "os"
)

func main() {
    resp, err := http.Get("http://www.jakas-strona.pl")
    if err != nil {
        fmt.Fprintf(os.Stderr, "nie można otworzyć strony: %v\n", err)
        os.Exit(1)
    }

    doc, err := html.Parse(resp.Body)
    resp.Body.Close()
    if err != nil {
        fmt.Fprintf(os.Stderr, "nie można przeanalizować składni: %v\n", err)
        os.Exit(1)
    }

    // Wydrukujmy tytuł strony.
    fmt.Printf("Tytuł: %s\n", traverse(doc))
}

// Veni, vidi, vici.
func traverse(n *html.Node) string {
    if n.Type == html.ElementNode && n.Data == "title" {
        return n.FirstChild.Data
    }

    for c := n.FirstChild; c != nil; c = c.NextSibling {
        title := traverse(c)
        if title != "" {
            return title
        }
    }

    return ""
}
```

Przykładowe wyjście:

```
Tytuł: Strona główna - www.jakas-strona.pl
```

## Bardziej szczegółowo

Analiza składni HTML datuje się na początki powstawania HTML jako języka znaczników. Do tej pory powstało wiele bibliotek i narzędzi do analizy składni HTML, zarówno w Go, jak i innych językach programowania.

Alternatywą do `golang.org/x/net/html` mogą być takie biblioteki jak `goquery`, które udostępniają API podobne do jQuery, czy `colly`, efektywna biblioteka do scrapingu stron.

Warto zaznaczyć, że choć analiza składni HTML jest niezbędna dla wielu zastosowań, nie zawsze jest to najbardziej efektywne rozwiązanie. Często lepszym podejściem może być skorzystanie z API strony, jeśli taka opcja jest dostępna.

## Zobacz też

* [Goquery](https://github.com/PuerkitoBio/goquery) - jQuery-like syntax in Go
* [Colly](http://go-colly.org/) - Elegant Scraper and Crawler Framework for Golang.
* [Documentacja golang.org/x/net/html](https://pkg.go.dev/golang.org/x/net/html) - Oficjalna dokumentacja modułu HTML Go.
* [Analiza składni HTML na Wikipedia](https://pl.wikipedia.org/wiki/Parsing) - Wikipedia entry on HTML parsing.