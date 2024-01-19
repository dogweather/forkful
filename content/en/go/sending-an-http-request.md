---
title:                "Sending an http request"
html_title:           "Bash recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

HTTP requests are a fundamental part of web applications, used to fetch data from another source, like a database or another server. They allow programmers to send or receive data, manipulate remote resources, and more.

## How to:

Below is a sample code that creates a simple GET request in Go:

```Go
package main

import (
	"net/http"
	"io/ioutil"
	"fmt"
)

func main() {
	resp, err := http.Get("http://webcode.me")
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
When you run this program, it sends a GET request to the specified URL and prints the response. 

## Deep Dive

Go's `http` package has been around since Go 1 release in March 2012, proving to be a robust and efficient tool for programmers. It provides HTTP client and server implementations for building and sending HTTP requests.

Though it's widely used, Go's `http` package isn't your only option for sending HTTP requests in Go. Alternatives include the `httpclient` and `go-resty` packages, but they come with their own pros and cons. 

While `http.Get` is convenient for simple GET requests, for more complex needs, you might use `http.NewRequest` and `http.Client.Do`.

Here's an example:

```Go
req, err := http.NewRequest("GET", "http://webcode.me", nil)
if err != nil {
	// handle err
}
client := &http.Client{}
resp, err := client.Do(req)
if err != nil {
	// handle err
}
```

That allows more control over the request and dealing with the response.

## See Also

For further reading, 

- Official Golang Net/Http Documentation: https://pkg.go.dev/net/http
- Effective Go: https://golang.org/doc/effective_go
- Go’s http package by example: https://go.dev/blog/http2