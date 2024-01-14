---
title:                "Go recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests is a crucial aspect of web development, as it allows programs to communicate with servers and retrieve data. It is also the foundation for building applications that integrate with APIs, which are essential for modern web development.

## How To

To send an HTTP request in Go, we first need to import the "net/http" package. Then, we can use the "http.Get()" function to make a GET request to a specific URL. Here's an example:

```Go
package main

import (
    "fmt"
    "net/http"
)

func main() {
    res, err := http.Get("https://example.com")
    if err != nil {
        fmt.Println("Error:", err)
    }
    fmt.Println(res.Status)
}
```

In this code, we use the "http.Get()" function to make a GET request to "https://example.com". The function returns two values: the response and any error that may occur during the request. We then print the response status, which in this case would be "200 OK" if the request was successful.

Aside from "http.Get()", there are other functions from the "net/http" package that can be used for different types of HTTP requests. For example, to send a POST request, we can use "http.Post()" instead. It is important to handle any errors that may occur during the request, as shown in the above example.

## Deep Dive

When we make an HTTP request, the request is sent to the server, and the server responds with a status code and a body. The status code is a 3-digit number that indicates whether the request was successful or if there was an error. The body of the response contains any data that the server may have sent back.

There are also different types of headers that can be included in an HTTP request, such as "Content-Type" and "Authorization". These headers provide additional information about the request to the server. It is essential to understand how to use headers correctly when making HTTP requests in Go.

## See Also

Here are some additional resources for learning about sending HTTP requests in Go:

- [The Net/HTTP Package in Go](https://golang.org/pkg/net/http/)
- [HTTP Requests in Go](https://www.digitalocean.com/community/tutorials/how-to-make-http-requests-in-go)
- [Making HTTP Requests in Go](https://blog.golang.org/error-handling-and-go)