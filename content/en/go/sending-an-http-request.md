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

## Why

Sending HTTP requests is a fundamental aspect of web development and allows developers to interact with various web services and APIs. Learning how to send HTTP requests in Go can greatly enhance your skills as a programmer and enable you to build more robust and versatile applications.

## How To

To send an HTTP request in Go, we can use the built-in `net/http` package. Here's a simple example of making a GET request to the Google homepage:

```
package main

import (
    "fmt"
    "net/http"
    "io/ioutil"
)

func main() {
    // Create a new request
    req, err := http.NewRequest("GET", "https://www.google.com", nil)
    if err != nil {
        // Handle error
    }

    // Send the request and get the response
    client := &http.Client{}
    resp, err := client.Do(req)
    if err != nil {
        // Handle error
    }
    defer resp.Body.Close()

    // Read the response body
    body, err := ioutil.ReadAll(resp.Body)
    if err != nil {
        // Handle error
    }

    // Print the response body
    fmt.Println(string(body))
}
```

Output:
```
<!doctype html>
<html>
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width,initial-scale=1">
    <title>Google</title>
    <!-- Rest of the HTML code -->
</body>
</html>
```

Sending a POST request is similar, but we need to specify the request body and content type:

```
// Create a new request
req, err := http.NewRequest("POST", "https://www.someapi.com/users", strings.NewReader(`{"name": "John", "age": 25}`))
if err != nil {
    // Handle error
}

// Set the content type
req.Header.Set("Content-Type", "application/json")

// Send the request and get the response
// Rest of the code is the same as above
```

## Deep Dive

The `net/http` package offers a lot of flexibility and options for sending HTTP requests. For example, we can set headers, cookies, and custom redirects using methods like `req.Header.Set()`, `req.AddCookie()`, and `client.CheckRedirect()`. We can also send different types of requests such as GET, POST, PUT, DELETE, HEAD, and PATCH by specifying the method in `http.NewRequest()`. Understanding these options can help us make more specialized and efficient requests.

It's also worth noting that the `net/http` package automatically follows redirects by default. However, this can be changed by setting `client.CheckRedirect` to a custom function that handles redirects according to our needs.

## See Also

- [Go Net/HTTP Example for Get and Post](https://golangbyexample.com/http-get-post-request/)
- [Sending HTTP requests using Golang](https://dev.to/usamaashraf/http-requests-in-golang-23kk)
- [Making HTTP requests in Go](https://dzone.com/articles/how-to-make-http-requests-in-go)