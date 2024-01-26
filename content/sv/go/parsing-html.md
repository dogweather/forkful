---
title:                "Tolka HTML"
date:                  2024-01-20T15:32:07.264981-07:00
html_title:           "Arduino: Tolka HTML"
simple_title:         "Tolka HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-html.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Parsing HTML innebär att du läser och tolkar kod från HTML-dokument så att program kan förstå och manipulera den. Programmerare gör detta för att skrapa data, manipulera innehåll eller automatisera interaktioner med webbsidor.

## How to:
```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"strings"
)

func main() {
	resp, err := http.Get("https://example.com")
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
Sample Output:
```
https://www.iana.org/domains/example
```

## Deep Dive
HTML-parsning i Go har genomgått några förändringar. Ursprungligen, användes regexp och andra hemmagjorda lösningar, men de var bristfälliga och utsatta för fel. Go's "x/net/html" paketet gör det robust och enklare att navigera DOM-trädet. Alternativ till Go's standardbibliotek, som "goquery", finns för jQuery-lik syntax, vilket kan kännas bekant för de flesta. Paketet "colly" erbjuder ett högre nivås gränssnitt för webbskrapning.

Det är viktigt att komma ihåg att HTML parsing och web scraping kan vara juridiskt känsligt, beroende på hur och var det används. Respektera robot.txt filer och använd gränssnitten ansvarsfullt.

## Se även
- Go's officiella dokumentation för "net/html" paketet: https://pkg.go.dev/golang.org/x/net/html
- "goquery": https://github.com/PuerkitoBio/goquery
- "colly", en kraftfull Go web scraping framework: http://go-colly.org/
- En introduktion till web scraping med Go: https://blog.golang.org/2015/01/01/making-and-new-years-resolution
- Webbaserad kurs i Go programmering: https://gophercises.com/
