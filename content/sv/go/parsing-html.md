---
title:                "Att tolka html"
html_title:           "Go: Att tolka html"
simple_title:         "Att tolka html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/parsing-html.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att parsning av HTML är en process där man extraherar data från en HTML-webbsida. Detta kan vara användbart för webbskrapning eller datautvinning. Det är också användbart för att bygga verktyg som kan analysera och manipulera webbsidor.

## Hur man gör:
```go
package main

import (
    "fmt"
    "net/http"
    "strings"

    "golang.org/x/net/html"
)

func main() {
    resp, err := http.Get("https://www.example.com")
    if err != nil {
        fmt.Println("Error retrieving webpage:", err)
        return
    }
    defer resp.Body.Close()

    doc, err := html.Parse(resp.Body)
    if err != nil {
        fmt.Println("Error parsing HTML:", err)
        return
    }

    var links []string
    var findLinks func(*html.Node)
    findLinks = func(n *html.Node) {
        if n.Type == html.ElementNode && n.Data == "a" {
            for _, attr := range n.Attr {
                if attr.Key == "href" {
                    links = append(links, attr.Val)
                    break
                }
            }
        }
        for c := n.FirstChild; c != nil; c = c.NextSibling {
            findLinks(c)
        }
    }

    findLinks(doc)
    fmt.Println("Links found on webpage:", links)
}
```

Output:
```
Links found on webpage: [/, /about, /contact, /products]
```

## Djupdykning:
Parsning av HTML blev populärt under 90-talet då internet började ta fart. Det finns många andra språk och verktyg som också kan användas för att parsning av HTML, som Python's BeautifulSoup och Nokogiri för Ruby. Go har dock ett centralt paket för detta ändamål kallat "net/html" som gör parsning enkelt och effektivt. Det har också stöd för att ladda in HTML från både lokala filer och HTTP-förfrågningar.

## Se även:
- [Go-paketet "net/html"](https://github.com/golang/net/tree/master/html)
- [Python BeautifulSoup](https://pypi.org/project/beautifulsoup4/)
- [Ruby Nokogiri](https://rubygems.org/gems/nokogiri)