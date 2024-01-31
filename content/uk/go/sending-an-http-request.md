---
title:                "Надсилання HTTP-запиту"
date:                  2024-01-20T17:59:41.955712-07:00
model:                 gpt-4-1106-preview
simple_title:         "Надсилання HTTP-запиту"

category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Sending an HTTP request means reaching out to a web server for data or action. Programmers do it to interact with APIs, fetch web content, or communicate between services.

## How to: (Як це зробити:)
Here's a basic GET request:

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

And just like that, you should see `example.com`'s HTML in your console.

## Deep Dive (Поглиблений Занурення)
HTTP requests in Go were popularized by the `net/http` package, efficient and robust for web interactions. Alternatives include using `curl` in Unix systems or libraries like `Resty` for simpler syntax. Under the hood, Go manages details like TCP/IP protocols and thread safety, simplifying the process for developers.

## See Also (Дивіться також):
- Go `net/http` package documentation: https://pkg.go.dev/net/http
- `ioutil` package, for reading the response: https://pkg.go.dev/io/ioutil
- Go by Example, HTTP clients: https://gobyexample.com/http-clients
- Official Go blog, "Go's HTTP client and server": https://blog.golang.org/http
