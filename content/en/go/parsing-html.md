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

## What & Why?

Parsing HTML is the process of converting the HTML code of a webpage into a structured format that can be easily read and manipulated by a programming language. This is necessary for programmers because it allows them to extract specific information from the webpage, such as text or images, and use it in their code.

## How to:

Go has a built-in package called "html" that provides functions for parsing HTML. Let's take a look at an example of how to use it:

```Go
// Import the html package
import "html"

// Create a variable with the HTML code of a webpage
htmlCode := `<h1>Hello World</h1>`

// Use the html.Parse function to convert the HTML code into a *Node type
node, err := html.Parse(strings.NewReader(htmlCode))
if err != nil {
    // Handle error
}

// Print the text content of the first <h1> element
fmt.Println(node.FirstChild.FirstChild.Data)

// Output: Hello World
```

In this example, the ```html.Parse()``` function takes in a string containing HTML code and returns a *Node type. This node can then be used to navigate through the different elements of the HTML code and extract desired information.

## Deep Dive

HTML, which stands for Hypertext Markup Language, is a markup language used to create webpages. It was first introduced in 1993 and has undergone several versions and updates since then. As the internet has evolved, HTML has become a crucial part of web development and is the standard format for creating web pages.

There are alternative ways to parse HTML, such as using regular expressions or third-party libraries. However, Go's built-in package "html" is a more efficient and reliable option as it is specifically designed for parsing HTML and follows the official HTML specification.

The ```html.Parse()``` function uses a parser called the HTML tokenizer, which breaks down the HTML code into tokens and creates a tree-like structure of nodes. These nodes represent the different elements of the webpage, such as headings, paragraphs, and images.

## See Also

To learn more about parsing HTML in Go, check out the official documentation for the "html" package: https://pkg.go.dev/html

You can also take a look at these resources for further reading:

- An introduction to HTML parsing with Go: https://flaviocopes.com/go-html-parse/
- Tutorial on parsing HTML with Go: https://www.alexedwards.net/blog/parsing-html-in-go
- A comparison of different approaches to parsing HTML in Go: https://blog.owulveryck.info/2016/01/28/parsing-html-in-go-the-lexicographic-solution.html