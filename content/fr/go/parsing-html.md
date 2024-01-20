---
title:                "Analyse syntaxique de HTML"
date:                  2024-01-20T15:31:40.077072-07:00
html_title:           "Arduino: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Parsez du HTML, c'est comme lire le journal et chercher les gros titres, sauf qu'ici, c'est pour extraire des données du code HTML. Les développeurs font ça pour récupérer du contenu, tester des applis web, ou pour du web scraping.

## Comment faire :
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("https://exemple.com")
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

Output échantillon (varie en fonction du HTML de exemple.com) :
```
https://exemple.com/contact
https://exemple.com/about
```

## Plongée en profondeur
Par le passé, l'analyse de HTML était bordélique. Avec Go, c'est plus structuré grâce au package "golang.org/x/net/html" qui facilite la vie. Alternatives ? Il y a regexp pour les cas simples, mais c'est risqué. En détail, `html.Parse` transforme le contenu HTML en structures de donnée navigables, permettant d'inspecter, de chercher ou de manipuler des éléments HTML.

## Voir aussi :
- Documentation du package HTML de Go : https://pkg.go.dev/golang.org/x/net/html
- Tutoriel Go sur le web scraping : https://golangdocs.com/web-scraping-in-golang
- Pourquoi éviter regexp pour parsez HTML : https://stackoverflow.com/a/1732454