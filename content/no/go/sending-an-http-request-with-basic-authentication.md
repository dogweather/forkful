---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:01:33.100424-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Sending an HTTP request with basic authentication involves attaching a header with a username and a password to verify who's making the request. Programmers use this method to access resources that need identity confirmation.

## How to: (Slik gjør du:)
First, let's install the necessary package:

```Go
go get -u "net/http"
```

Now, the code:

```Go
package main

import (
	"encoding/base64"
	"fmt"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, _ := http.NewRequest("GET", "http://example.com", nil)
	
	username := "user"
	password := "pass"
	auth := username + ":" + password
	encodedAuth := base64.StdEncoding.EncodeToString([]byte(auth))
	req.Header.Add("Authorization", "Basic "+encodedAuth)

	resp, err := client.Do(req)
	if err != nil {
		fmt.Println(err)
		return
	}
	defer resp.Body.Close()

	fmt.Println("Status Code:", resp.StatusCode)
}
```

And the expected output upon success:

```Go
Status Code: 200
```

## Deep Dive (Dypdykk)
HTTP Basic Authentication has been around since the early days of the web. It's simple but not the most secure—credentials are not encrypted, just base64 encoded. Now, there's OAuth and JWT for more secure alternatives. For basic auth, the `Authorization` header holds the credentials. In Go, you use the `net/http` package to construct requests, and you manually add that header. The `base64` package encodes the username and password.

## See Also (Se Også)
- Official Go `net/http` package documentation: [https://golang.org/pkg/net/http/](https://golang.org/pkg/net/http/)
- Go `base64` encoding: [https://golang.org/pkg/encoding/base64/](https://golang.org/pkg/encoding/base64/)
- OAuth: [https://oauth.net/](https://oauth.net/)
- JSON Web Tokens (JWT): [https://jwt.io/](https://jwt.io/)
