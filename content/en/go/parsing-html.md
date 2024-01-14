---
title:                "Go recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-html.md"
---

{{< edit_this_page >}}

## Why

Have you ever come across a website and thought, "I wish I could extract specific information from this HTML code"? One of the many reasons for parsing HTML is data extraction. For example, you might want to scrape a webpage for specific data or extract links for web crawling.

## How To

Parsing HTML in Go is made easy with the help of third-party packages such as "golang.org/x/net/html". Here's a simple program to parse HTML and extract all the links on a webpage:

```Go
package main

import (
    "fmt"
    "log"
    "net/http"

    "golang.org/x/net/html"
)

func main() {
    url := "http://example.com"
    resp, err := http.Get(url)
    if err != nil {
        log.Fatal(err)
    }

    doc, err := html.Parse(resp.Body)
    if err != nil {
        log.Fatal(err)
    }

    links := getLinks(doc)

    for _, link := range links {
        fmt.Println(link)
    }
}

func getLinks(n *html.Node) []string {
    var links []string

    if n.Type == html.ElementNode && n.Data == "a" {
        for _, attr := range n.Attr {
            if attr.Key == "href" {
                links = append(links, attr.Val)
            }
        }
    }

    for c := n.FirstChild; c != nil; c = c.NextSibling {
        links = append(links, getLinks(c)...)
    }

    return links
}
```

Sample output for the "example.com" webpage:

```
http://anotherexample.com
http://yetanotherexample.com
```

This is just a simple example, but the possibilities are endless with HTML parsing in Go.

## Deep Dive

Parsing HTML involves traversing through the DOM (Document Object Model) tree and extracting the desired elements. The "golang.org/x/net/html" package provides functions such as "Parse" and "NewTokenizer" to help with this process.

Another useful package for parsing HTML in Go is "github.com/PuerkitoBio/goquery". It allows you to use CSS selectors to easily extract specific elements from a webpage.

It's important to keep in mind that parsing HTML can be a complex task. The HTML structure of different websites can vary greatly, so it's important to have a good understanding of HTML before attempting to parse it. Additionally, websites can change their structure at any time, so it's important to regularly test and update your parsing code as needed.

## See Also

To learn more about parsing HTML in Go, check out these helpful resources:

- [Official Go documentation for the "golang.org/x/net/html" package](https://godoc.org/golang.org/x/net/html)
- [Tutorial: Parsing HTML in Go using "golang.org/x/net/html"](https://www.calhoun.io/parsing-html-with-go/)
- [Article: Web Scraping with Go](https://scrapethissite.com/scraping-101-with-go/)