---
title:                "Downloading a web page"
html_title:           "Bash recipe: Downloading a web page"
simple_title:         "Downloading a web page"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?
Downloading a webpage is the process of pulling down data from a specific URL to your machine. This is something programmers often do when building web scrapers or bots, or when testing their own sites for performance and compatibility.

## How to:
In Go, we use the `net/http` package to make this happen. Here, I'll download Google's homepage and print it:

```Go
package main

import (
	"io/ioutil"
	"net/http"
	"fmt"
)

func main() {
	resp, err := http.Get("http://www.google.com")
	if err != nil {
		panic(err)
	}
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Println(string(body))
}
```
In this example, we send a GET request to www.google.com using `http.Get`. Then, we read the response body into `body` variable and print it out. If errors occur, we use `panic` to fail immediately.

## Deep Dive
This technique of downloading a page has been around since the early days of the internet when pages were simple HTML. Today, it's often not as simple as just pulling down a webpage due to dynamic content rendered on the client-side with JavaScript. 

Alternatives to simple HTTP get requests are numerous, and selection should be based on the complexity of the page. You might need to use something like Selenium WebDriver if you're dealing with complex and highly interactive webpages.

Implementation-wise, `http.Get` in our example is pretty straightforward. It establishes an HTTP connection and returns a pointer to the `Response` which contains the server's response to the HTTP request. `ioutil.ReadAll` is reading the response body. It's important to always check for errors when making network calls and handle them appropriately to avoid crashes and undefined behavior in Go.

## See Also
- [`net/http` package documentation](http://golang.org/pkg/net/http/)
- [A more advanced web scraper written in Go](https://github.com/gocolly/colly)
- [Selenium WebDriver documentation](https://www.selenium.dev/documentation/en/)
- [An example of using Selenium with Go](https://github.com/tebeka/selenium)