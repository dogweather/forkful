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

## What & Why?

Sending an HTTP request with basic authentication is a way for programmers to secure their web requests by requiring a username and password before accessing a certain resource. This is commonly used for authentication purposes, such as when accessing APIs or other sensitive data.

## How to:

Go makes it easy to send an HTTP request with basic authentication with its built-in `net/http` package. First, we need to set up a `Client` object and specify the username and password in the `BasicAuth` method. Then, we can use the `Get` method to send a GET request and get the response back.

```Go
// Set up the Client
client := &http.Client{}

// Set username and password
username := "myusername"
password := "mypassword"

// Set up the request and add basic authentication
req, err := http.NewRequest("GET", "https://api.example.com/users", nil)
req.SetBasicAuth(username, password)

// Send the request and get the response
resp,err := client.Do(req)
defer resp.Body.Close()

// Print out the response
body, err := ioutil.ReadAll(resp.Body)
fmt.Println(string(body))
```

The output should be the response from the API call, which could be in the form of JSON or any other format depending on the API.

## Deep Dive

Basic authentication has been around since the early days of the web and is still widely used today. It was originally introduced as a way to protect resources and restrict access to authorized users. However, due to its simplicity, it is not considered the most secure form of authentication.

There are alternatives to basic authentication, such as OAuth, which is a more secure and widely adopted method for authentication. Another alternative is API keys, where a unique key or token is generated for each user and is used to authenticate their requests.

In terms of implementation, basic authentication works by adding the username and password in the `Authorization` header of the HTTP request in the format `Basic username:password` which is then encoded in Base64. This means that the username and password are transmitted in plaintext and can be easily decoded, making it vulnerable to security threats.

## See Also

For more information on sending HTTP requests with basic authentication in Go, check out these resources:

- [Official Documentation on Net/HTTP Package](https://golang.org/pkg/net/http/#Request.SetBasicAuth)
- [A Beginner's Guide to HTTP Authentication](https://www.smashingmagazine.com/2018/01/understanding-using-rest-api/)
- [OAuth vs. Basic Authentication](https://nordicapis.com/why-oauth-is-taking-over-rest-api-security-trends/)