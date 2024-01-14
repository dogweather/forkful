---
title:                "Go recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Why

Sending HTTP requests with basic authentication is a crucial aspect of building web applications. It allows you to securely transmit sensitive data and authenticate users before granting them access to certain resources. In this blog post, we will explore how to send an HTTP request with basic authentication in Go.

## How To

To send an HTTP request with basic authentication in Go, we will use the built-in `net/http` package. This package provides methods for making HTTP requests and handling responses. First, we need to import the package into our code:

```Go
import "net/http"
```

Next, we need to specify the URL of the web service we want to access and the necessary credentials for basic authentication. We do this by creating an `http.Client` and adding the authentication details to the `Transport`:

```Go
client := &http.Client{
    Transport: &http.Transport{
        Proxy: http.ProxyFromEnvironment,
        DialContext: (&net.Dialer{
            Timeout:   30 * time.Second,
            KeepAlive: 30 * time.Second,
        }).DialContext,
        TLSClientConfig: &tls.Config{
            InsecureSkipVerify: true,
        },
    },
}
req, err := http.NewRequest("GET", "https://example.com/api", nil)
req.SetBasicAuth("username", "password")
```

We then make the request using the `Do()` method of the HTTP client:

```Go
res, err := client.Do(req)
if err != nil {
    panic(err)
}
```

The response can be read using the `Response` fields, such as `Body`, `StatusCode`, and `Header`.

```Go
fmt.Println("Response Status:", res.Status)
data, err := ioutil.ReadAll(res.Body)
res.Body.Close()
if err != nil {
    panic(err)
}
fmt.Printf("Response Body: %s\n", data)
```

Running this code will result in the following output:

```
Response Status: 200 OK
Response Body: {"id": 1, "name": "John Doe"}
```

## Deep Dive

Behind the scenes, the basic authentication process involves adding the `Authorization` header to the HTTP request. This header contains the word "Basic" followed by a base64-encoded string of the format "username:password". This encoding ensures that the username and password are not transmitted in plain text.

In the code example above, we used the `SetBasicAuth()` method to add this header to our request. We could have also added it manually by using the `Set()` method:

```Go
auth := "Basic " + base64.StdEncoding.EncodeToString([]byte("username:password"))
req.Header.Set("Authorization", auth)
```

It is important to note that basic authentication is not considered secure as the credentials are transmitted in every request and can be easily decoded. It is recommended to use more secure methods of authentication, such as OAuth or API keys.

## See Also

- [Go by Example: HTTP Clients](https://gobyexample.com/http-clients)
- [Official net/http package documentation](https://golang.org/pkg/net/http/)
- [Basic Authentication on Wikipedia](https://en.wikipedia.org/wiki/Basic_access_authentication)