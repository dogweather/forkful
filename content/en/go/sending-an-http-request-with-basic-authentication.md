---
title:                "Sending an http request with basic authentication"
html_title:           "Fish Shell recipe: Sending an http request with basic authentication"
simple_title:         "Sending an http request with basic authentication"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Send HTTP Request with Basic Auth in Go

## What & Why?

Sending an HTTP request with basic authentication is a way to secure and grant access to certain parts of a web service. Programmers do this to protect sensitive data from unauthorized use while providing controlled access to legit users.

## How to:

Here's an example of sending a GET HTTP request with basic authentication:

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	req, err := http.NewRequest("GET", "https://api.github.com/user", nil)
	if err != nil {
		fmt.Printf("http.NewRequest() error: %v\n", err)
		return
	}
	req.SetBasicAuth("username", "password")

	client := &http.Client{}
	resp, err := client.Do(req)
	if err != nil {
		fmt.Printf("http.Do() error: %v\n", err)
		return
	}
	defer resp.Body.Close()
  
	// ... read resp.Body here...
}
```
In the above code, replace 'username' and 'password' with your own credentials.

## Deep Dive

Historically, basic authentication was a simple protocol, where user credentials were encoded and transmitted with each HTTP request. It's still widely used due to its simplicity, despite this method's potential vulnerability, as credentials can be intercepted easily.

Alternatives include OAuth and Token-based auth. They work by issuing tokens which are sent as headers in the HTTP request, and are generally more secure compared to Basic Auth.

In Go, an `http.Request` object's method `SetBasicAuth(username, password string)` is used to add the Authorization header to the HTTP request. It encodes the username and password in base64 and prepends `Basic ` to it.

## See Also

To build upon this, checkout [Go's full documentation on net/http](https://golang.org/pkg/net/http/) package. For secure alternatives, [OAuth 2.0](https://oauth.net/2/), or [JWT](https://jwt.io/) can be good starting points. Learn more about basic authentication from the [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication).