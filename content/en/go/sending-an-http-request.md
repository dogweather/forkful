---
title:                "Sending an HTTP request"
date:                  2024-01-20T17:59:35.958908-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?
Sending an HTTP request is how your program asks another system for data or sends data to it. Programmers do this to interact with web services, APIs, and to exchange information across the internet.

## How to:
Here's a snippet in Go for sending a GET request and handling the response:

```Go
package main

import (
	"io"
	"log"
	"net/http"
	"os"
)

func main() {
	response, err := http.Get("https://api.example.com/data")
	if err != nil {
		log.Fatal(err)
	}
	defer response.Body.Close()

	if response.StatusCode == http.StatusOK {
		body, readErr := io.ReadAll(response.Body)
		if readErr != nil {
			log.Fatal(readErr)
		}
		os.Stdout.Write(body)
	} else {
		log.Printf("Received non-OK response status: %s", response.Status)
	}
}
```

Here's what you might see after running this:

```
{"name":"John Doe","occupation":"Software Developer"}
```

## Deep Dive

Before Go's `net/http` package made life easier, sending HTTP requests was a pain. Early days had us dealing with low-level socket programming which was a lot about managing TCP connections and protocols manually. Today, the standard library abstracts these complexities.

While `http.Get` is handy for simple requests, when you need more control, `http.NewRequest` and `http.Client` are your pals. They let you modify headers, set timeouts, and handle redirects more precisely.

A point to ponder: `http.Get` and its pals are blocking calls. They don't return until the HTTP response is fully received. In a high-traffic app, use Go's concurrency features like goroutines and channels to avoid slowing down.

Alternatives include third-party packages like `Resty` or `GoReq`. Some prefer them for their fluent interfaces and extra functionality. Always consider if the benefits outweigh the cost of adding a dependency.

## See Also

- The Go net/http package documentation: [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- Effective Go – Concurrency: [https://golang.org/doc/effective_go#concurrency](https://golang.org/doc/effective_go#concurrency)
- Go by Example – HTTP Clients: [https://gobyexample.com/http-clients](https://gobyexample.com/http-clients)
- "The Go Programming Language" book for an in-depth understanding of Go's standard library.
