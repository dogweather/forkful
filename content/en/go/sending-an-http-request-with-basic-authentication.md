---
title:                "Sending an http request with basic authentication"
html_title:           "Go recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why: Securing Your HTTP Requests

Sending HTTP requests with basic authentication allows you to secure your requests by adding an additional layer of authentication. This is especially important when making sensitive requests to a server.

## How To: Coding with Go

To send an HTTP request with basic authentication using Go, we can use the `net/http` package. First, we need to create an `http.Client` and set the username and password for basic authentication.

```
client := &http.Client{}
username := "example"
password := "password"
```

Next, we use the `http.NewRequest` function to create a new `http.Request` object, passing in the HTTP method, URL, and request body if needed. Then, we use the `SetBasicAuth` method on the `Request` object to add the basic authentication credentials.

```
req, err := http.NewRequest("GET", "https://example.com/api", nil)
req.SetBasicAuth(username, password)
```

We can then use the client's `Do` method to send the request, and handle any errors that may occur.

```
resp, err := client.Do(req)
if err != nil {
    fmt.Println("Error sending request:", err)
}
```

To retrieve the response body, we can use the `ioutil` package and the `ReadAll` function to read the response body into a `[]byte`:

```
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    fmt.Println("Error reading response:", err)
}
fmt.Println(string(body))
```

## Deep Dive: Understanding Basic Authentication

Basic authentication adds an `Authorization` header to the HTTP request with the value of `Basic` followed by the Base64 encoded username and password, separated by a colon. This provides a simple, but not very secure, way to authenticate requests.

Some additional considerations when using basic authentication are:

- Basic authentication only encrypts the username and password, not the entire request, making it susceptible to man-in-the-middle attacks.
- The username and password are passed in plain text, so it is important to use HTTPS when using basic authentication.
- If the username and password are not properly secured, they can be easily decoded from the Base64 encoding.

## See Also

- [net/http package documentation](https://golang.org/pkg/net/http/)
- [Basic authentication in depth](https://en.wikipedia.org/wiki/Basic_access_authentication)