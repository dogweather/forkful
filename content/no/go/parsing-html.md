---
title:                "Analysering av html"
html_title:           "C#: Analysering av html"
simple_title:         "Analysering av html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/parsing-html.md"
---

{{< edit_this_page >}}

# Å Parse HTML i Go

## Hva & Hvorfor?
Parsing av HTML er prosessen med å konvertere HTML markups til strukturer som kan forstås av programmer. Dette gjør vi programmerere for å hente informasjon effektivt og organisere data på en strukturert måte fra HTML koder.

## Hvordan:
Her er et grunnleggende eksempel i Go for å parse en HTML-streng med Go’s innebygde "net/html" pakke.
```Go
pakkeringen hoved

importer(
	"fmt"
	"golang.org/x/net/html"
	"strenger"
)

funksjon hoved() {
	s := "<p forname='link'>Hei, <b>verden</b>!</p>"
	doc, err := html.Parse(strenger.NewReader(s))
	if err != nil {
		panic(err)
	}
	var f funksjonen(n *html.Node) {
		if n.Type == html.ElementNode {
			fmt.Println(n.Data, ": ", n.Attr)
		}
		for c := n.FirstChild; c != nil; c = c.NextSibling {
			f(c)
		}
	}
	f(doc)
}
```
Når du kjører denne koden, vil utdataene se slik ut:
```Go
p :  [{ fornavn link }]
b :  []
```
## Dyp Dykk
**Historisk kontekst**: Parsing av HTML i programmering har vært anvendt siden tiden for web scraping, for å manipulere og hente data fra internett-sider. 

**Alternativer**: Det er mange biblioteker tilgjengelig for parsing av HTML som 'BeautifulSoup', 'lxml', 'html.parser' i andre programmeringsspråk som Python, men Go sin 'net/html' pakke gir en god og enkel måte å parse HTML på.

**Implementeringsdetalj**: I praksis kan HTML parsing bli kompleks avhengig av hvor sammensatt HTML-koden er. Men Go's innebygde 'net/html' pakke forenkler denne kompleksiteten ved å behandle HTML's hierarkiske DOM-struktur som en rekke noder.

## Se Også
- Hvis du ønsker å fordype deg enda mer innen HTML parsing, kan du sjekke ut denne artikkelen: [Advanced HTML parsing in Go](https://medium.com/@ankurgel/working-with-go-1-11-4ca098a178a).
- For en mer detaljert veiledning i HTML parsing i andre programmeringsspråk, sjekk ut disse linkene: [Parsing HTML in Python](https://realpython.com/python-web-scraping-practical-introduction/), [Parsing HTML in Javascript](https://javascript.info/dom-parsing).