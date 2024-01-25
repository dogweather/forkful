---
title:                "Parsing HTML"
date:                  2024-01-20T15:31:44.542017-07:00
html_title:           "Bash recipe: Parsing HTML"
simple_title:         "Parsing HTML"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing HTML means extracting info from an HTML file – that's the code behind web pages. Programmers do it to automate data retrieval, extract content, and migrate content between systems.

## How to:
Go has a `net/html` package perfect for diving into HTML. Here’s the gist:

```Go
package main

import (
	"fmt"
	"golang.org/x/net/html"
	"net/http"
	"os"
)

func main() {
	// Fetch HTML
	resp, err := http.Get("http://example.com")
	if err != nil {
		fmt.Fprintf(os.Stderr, "fetch: %v\n", err)
		os.Exit(1)
	}
	defer resp.Body.Close()
	
	// Parse HTML
	doc, err := html.Parse(resp.Body)
	if err != nil {
		fmt.Fprintf(os.Stderr, "parse: %v\n", err)
		os.Exit(1)
	}

	// Traverse the HTML node tree
	var f func(*html.Node)
	f = func(n *html.Node) {
		if n.Type == html.ElementNode && n.Data == "a" {
			for _, a := range n.Attr {
				if a.Key == "href" {
					fmt.Printf("%v\n", a.Val)
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

Run it? You’ll get links from `example.com`, printed to your console. Easy!

## Deep Dive:
Here's the scoop. When the web was a newborn, HTML was simple. Not anymore. Today, it's complex, loaded with nuances. 

Why not regex? HTML can be inconsistent. Regex for HTML is a flaky, error-prone approach. Parsers like `net/html` are smarter. They handle oddities and nestings in HTML that would break a regex pattern.

The `net/html` parser builds a tree from HTML elements. It's like giving structure to a jumble of branches—turning chaos into something you can climb. You traverse the tree with your own functions to sift through tags and attributes.

What else could you use? Libraries like `goquery` give a jQuery-like experience for Go, and `colly` is a popular choice for scraping.

## See Also:
- Go's `net/html` package: https://pkg.go.dev/golang.org/x/net/html
- GoQuery for a jQuery-like syntax: https://github.com/PuerkitoBio/goquery
- Colly for scraping: http://go-colly.org/
