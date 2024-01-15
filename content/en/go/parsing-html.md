---
title:                "Parsing html"
html_title:           "Go recipe: Parsing html"
simple_title:         "Parsing html"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/parsing-html.md"
---

{{< edit_this_page >}}

## Why

If you've ever worked with web scraping or data extraction, chances are you've encountered HTML. This markup language is used to create the structure and content of web pages. So why would someone want to parse HTML? Well, parsing HTML allows you to extract specific data from a web page, making it easier to analyze and use for various purposes.

## How To

To parse HTML in Go, you will need to use a package called "html". Here's a simple example of how to use it:

```Go
// Import the html package
import "golang.org/x/net/html"

// Create a string variable containing the HTML
var htmlString = "<h1>Hello World</h1>"

// Parse the HTML
document, _ := html.Parse(strings.NewReader(htmlString))

// Print the first H1 element
fmt.Println(document.FirstChild.FirstChild.Data)
```

In the above code, we first import the "html" package, which contains functions for parsing HTML. Then, we create a string variable containing the HTML we want to parse. Next, we use the `html.Parse` function to create a document from our HTML string. Finally, we use the `FirstChild` property to access the first child element of the document, in this case, the first H1 element. 

The output of the above code would be:

```
Hello World
```

You can also use the `html.ParseFragment` function to parse a specific element from the HTML, instead of the whole document. Here's an example:

```Go
// Parse only the first H1 element
fragment, _ := html.ParseFragment(strings.NewReader(htmlString), nil)

// Print the first H1 element
fmt.Println(fragment[0].FirstChild.Data)
```

The output would be the same as before.

## Deep Dive

When parsing HTML, it's important to understand the structure of the document. HTML is made up of elements which can have attributes and child elements. A document is typically structured like a tree, with the `html` element at the root, and all other elements branching out from there. 

To access specific elements, you can use the `FirstChild` and `NextSibling` properties. These properties allow you to traverse through the document, accessing each element as you go. For more detailed information on working with HTML in Go, check out the official documentation for the "html" package.

## See Also

- [Official "html" package documentation](https://pkg.go.dev/golang.org/x/net/html)
- [Guide to web scraping in Go](https://towardsdatascience.com/web-scraping-in-go-e8dfd5f298bc)
- [Tutorial on using the "html" package to parse HTML](https://opensourceschools.org/2019/07/08/go-html-parsing/)