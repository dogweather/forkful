---
title:                "Parsing html"
html_title:           "Gleam recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing HTML: developers dissect web pages to extract tidbits, essence, logic. Why? Automate tasks, extract data, insert content – you name it. 

## How to:
Now let's delve into Go code. Install the `goquery` package using the following command:

```Go
go get -u github.com/PuerkitoBio/goquery
```

An example on how to parse an HTML and extract text:

```Go
package main

import (
	"fmt"
	"log"

	"github.com/PuerkitoBio/goquery"
)

func main() {
	doc, err := goquery.NewDocumentFromReader(res.Body)
	if err != nil {
		log.Fatal(err)
	}

	// Find the review items
	doc.Find(".review-item").Each(func(i int, s *goquery.Selection) {
		// For each item found, get the review title
		title := s.Find(".review-title").Text()
		fmt.Printf("Review %d: %s\n", i, title)
	})
}
```

In the output, you can see the title of the reviews:

```
Review 0: Great product
Review 1: Could be better
...
```

## Deep Dive

Go's built-in `net/html` library introduced HTML parsing. `goquery` came next, making it simpler with jQuery-like syntax. 

Alternatives exist – `htmlquery`, package `html` from the `golang.org/x/net` package, etc. They have slight differences in syntax and usage. 

Underlying details? `goquery` employs Cascading Style-Style (CSS) selectors for traversing DOM nodes. The parsing itself is done using the tokenized approach, making it quite efficient.

## See Also 

1. Go documentation: [https://golang.org/pkg/net/html/](https://golang.org/pkg/net/html/)
2. Goquery documentation: [https://godoc.org/github.com/PuerkitoBio/goquery](https://godoc.org/github.com/PuerkitoBio/goquery)
3. Htmlquery documentation: [https://github.com/antchfx/htmlquery](https://github.com/antchfx/htmlquery)