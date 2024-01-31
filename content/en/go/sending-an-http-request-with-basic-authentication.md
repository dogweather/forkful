---
title:                "Sending an HTTP request with basic authentication"
date:                  2024-01-20T18:01:47.270757-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sending an HTTP request with basic authentication"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why?
HTTP requests with basic authentication add a simple security layer to an API call. Programmers use it to access resources requiring credentials, like user-specific data.

## How to:
Sending an authenticated HTTP request is straightforward in Go:

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "https://api.example.com/data", nil)
	if err != nil {
		panic(err)
	}

	username := "user"
	password := "pass"
	credentials := base64.StdEncoding.EncodeToString([]byte(username + ":" + password))
	req.Header.Add("Authorization", "Basic "+credentials)

	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	fmt.Printf("%s\n", body)
}
```

Sample output (with fictional API URL and credentials):
```plaintext
{"status":"success","data":"some private data"}
```

## Deep Dive
Basic authentication is part of the HTTP/1.0 spec and has been around since the early days of the web. It's not the most secure (credentials are just base64 encoded, not encrypted), so it's often swapped for OAuth or JWT in more sensitive applications.

Implementation-wise, Go includes built-in support for HTTP clients and requests, with package `net/http` enabling developers to handle web traffic. When using basic authentication, we need to ensure the credentials are appropriately encoded, and the `Authorization` header is added to the HTTP request.

Though simple, you should avoid using basic authentication over plain HTTP since it's susceptible to man-in-the-middle attacks. Always use HTTPS when you're sending credentials.

## See Also
- Go `net/http` package documentation: https://pkg.go.dev/net/http
- Go `encoding/base64` package documentation: https://pkg.go.dev/encoding/base64
- Info on HTTP Basic Authentication: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- For more secure authentication methods: https://oauth.net/ and https://jwt.io/
