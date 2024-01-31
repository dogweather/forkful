---
title:                "Downloading a web page"
date:                  2024-01-20T17:44:05.854805-07:00
model:                 gpt-4-1106-preview
simple_title:         "Downloading a web page"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why?

Downloading a web page means fetching its content through HTTP. Programmers do it to interact with web services, scrape data, or monitor site uptime.

## How to:

In Go, downloading a web page is a breeze with the `net/http` package. Here's the short of it:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}

	fmt.Println(string(body))
}
```

Run it, and you'll get the HTML of `http://example.com` slapped onto your screen, give or take some HTTP headers.

## Deep Dive

Back in the day, fetching web content was a wild west of socket programming and hand-crafted HTTP requests. Now, libraries like Go's `http` take the grunt work out of our hands.

Why not just `curl` or `wget`? Automation, my friend. Embedding the download logic in your code makes it repeatable and integratable.

Under the hood, `http.Get` makes a GET request, manages cookies, and more. You can control timeouts, headers, and go as deep as custom transports. But that's a story for another day.

As for alternatives, you might consider `http.Client` if you need more control, or third-party packages like `gorequest` for a different flavour.

## See Also

- The Go net/http package docs: https://pkg.go.dev/net/http
- Effective Go for best practices: https://golang.org/doc/effective_go
- Go by Example for more hands-on snippets: https://gobyexample.com/
