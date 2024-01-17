---
title:                "Sending an http request"
html_title:           "Go recipe: Sending an http request"
simple_title:         "Sending an http request"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why?

Sending an HTTP request is a way for a computer program to communicate with a web server. Programmers do this to retrieve data or perform actions on a server. It is an essential component of web development and allows for the creation of dynamic and interactive websites.

## How to:

```Go
// Import the "net/http" package
import "net/http"

// Create a new HTTP request with the specified method and URL
req, err := http.NewRequest("GET", "https://example.com/api/users", nil)

// Add any necessary headers
req.Header.Add("Content-Type", "application/json")

// Send the request using the "Client" from the "http" package
res, err := http.DefaultClient.Do(req)
if err != nil {
    // Handle error
}

// Print the response status code and body
fmt.Println(res.Status)
body, err := ioutil.ReadAll(res.Body)
if err != nil {
    // Handle error
}
fmt.Println(string(body))

// Close the response body
res.Body.Close()
```

Output:
```
200 OK
[{"id": 1, "name": "John", "email": "john@example.com"},{"id": 2, "name": "Jane", "email": "jane@example.com"}]
```

## Deep Dive

In the early days of the internet, communication between clients (such as web browsers) and servers relied on the Simple Mail Transfer Protocol (SMTP). However, as the internet evolved, the need for a more robust and versatile protocol became apparent. This led to the development of the Hypertext Transfer Protocol (HTTP) in 1991.

While there are alternatives to sending HTTP requests, such as using other protocols or libraries, HTTP remains the standard for web communication. In the Go programming language, sending HTTP requests is made simple with the built-in "net/http" package, which provides functions for creating and sending requests and handling responses.

When sending an HTTP request, the program first creates a "Request" object, specifying the method (e.g. GET, POST) and the URL of the server. Headers can be added to the request for additional information. The "Client" from the "net/http" package is then used to send the request and receive a response. The response can be accessed for information such as the status code and body.

## See Also

- [Go Documentation for "net/http" package](https://golang.org/pkg/net/http/)
- [HTTP Basics](https://developer.mozilla.org/en-US/docs/Web/HTTP/Basics)
- [HTTP vs HTTPS](https://www.cloudflare.com/en-gb/learning/ssl/https-vs-http/)