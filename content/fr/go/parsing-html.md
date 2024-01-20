---
title:                "Analyse syntaxique de HTML"
html_title:           "Bash: Analyse syntaxique de HTML"
simple_title:         "Analyse syntaxique de HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/go/parsing-html.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Parser HTML c'est analyser et transformer le code HTML en une structure de données plus facile à comprendre pour les programmes. Les programmeurs le font pour extraire et manipuler les données sur le web.

## Comment faire:
On peut utiliser le paquet `golang.org/x/net/html` pour parser du HTML. Voici un exemple simple:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, _ := http.Get("https://www.example.com")
	doc, _ := html.Parse(resp.Body)

	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.TextNode {
			text := strings.TrimSpace(n.Data)
			if len(text) > 0 {
				fmt.Println(text)
			}
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```
Ce programme extrait tout le texte de la page HTML, en ignorant les balises et les espaces superflus.

## Une exploration plus profonde

Le parser HTML de Go utilise une technique appelée analyseur syntactique récursive, une méthode couramment utilisée pour analyser des langages contextuels-libres tels que HTML.

Il existe d'autres approches pour parser du HTML en Go, comme `goquery` qui fournit une interface similaire à celle de jQuery. Cependant, `net/html` est souvent privilégié car il fait partie de la bibliothèque standard de Go et ne nécessite pas de dépendances externes.

En ce qui concerne les détails d'implémentation, Go utilise une machine à états pour gérer la complexité de l'analyse HTML. Cela permet au code de rester compréhensible et maintenable malgré le large éventail de scénarios possibles lors de l'analyse HTML.

## Voir aussi

Pour plus d'information:

- La documentation officielle du package `net/html`: https://golang.org/pkg/net/html/
- Le repository Github pour `goquery`: https://github.com/PuerkitoBio/goquery
- Un tutoriel sur le parsing HTML en Go: https://www.devdungeon.com/content/web-scraping-go