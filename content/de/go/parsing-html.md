---
title:                "Analysieren von HTML"
html_title:           "Go: Analysieren von HTML"
simple_title:         "Analysieren von HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/go/parsing-html.md"
---

{{< edit_this_page >}}

## Warum

HTML ist die gebräuchlichste Sprache zur Gestaltung von Webseiten und enthält wichtige Informationen über den Inhalt und die Struktur einer Webseite. Mit dem Parsen von HTML können Entwickler diese Informationen extrahieren und weiterverarbeiten, was für viele Anwendungen, von Web-Crawling bis hin zu datengetriebenen Entscheidungen, unerlässlich ist.

## Wie geht man vor?

Das Parsen von HTML ist mit Go relativ einfach. Zunächst muss man das entsprechende Paket importieren:

```Go
import "golang.org/x/net/html"
```

Anschließend kann man mithilfe der `html.Parse()` Funktion den HTML-Code in ein `*html.Node` Objekt parsen:

```Go
doc, err := html.Parse(resp.Body)
```

Hier sollte beachtet werden, dass `resp.Body` ein `io.Reader` Objekt sein muss, das den HTML-Code enthält. Mit diesem `*html.Node` Objekt kann nun durch die HTML-Struktur navigiert werden, um die gewünschten Informationen zu extrahieren. Hier ein Beispiel, um alle Links auf einer Seite zu finden:

```Go
func findLinks(n *html.Node) []string {
	var links []string
	if n.Type == html.ElementNode && n.Data == "a" {
		for _, attr := range n.Attr {
			if attr.Key == "href" {
				links = append(links, attr.Val)
			}
		}
	}
	for c := n.FirstChild; c != nil; c = c.NextSibling {
		links = append(links, findLinks(c)...)
	}
	return links
}
```

Dieser Code durchläuft rekursiv alle Nodes und sucht nach `<a>` Tags mit dem Attribut `href`. Die gefundenen Links werden in einem Slice gespeichert, das am Ende zurückgegeben wird.

## Tiefergehende Informationen

Das Parsen von HTML mit Go bietet auch die Möglichkeit, spezifische Elemente und Attribute gezielt zu suchen und auszulesen. Dazu können die Funktionen `Query()` und `Attr()` aus dem Paket `"golang.org/x/net/html"` verwendet werden.

Ein weiteres nützliches Werkzeug ist das Paket `"golang.org/x/net/html/atom"`, das häufig verwendete HTML-Elemente als vordefinierte Konstanten zur Verfügung stellt.

Außerdem bietet die offizielle Dokumentation von Go weitere Informationen und Beispiele zum Parsen von HTML.

## Siehe auch

- [Offizielle Dokumentation zu HTML-Parsing mit Go](https://golang.org/pkg/html)
- [Beispiele für das Parsen von HTML mit Go](https://gobyexample.com/html-parsing)
- [Tutorial: Web-Scraping mit Go](https://tutorialedge.net/golang/go-web-scraping-tutorial/)