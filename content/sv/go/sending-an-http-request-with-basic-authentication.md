---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Elixir: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Skicka HTTP-begäran med grundläggande autentisering i Go

## Vad & Varför?
Att skicka en HTTP-begäran med grundläggande autentisering innebär att vi skickar användarnamn och lösenord till servern som en del av begäran. Det görs för att säkerställa att endast behöriga användare kan få tillgång till resurser på en server.

## Hur:
Här är ett exempel på hur du skickar en HTTP-begäran med grundläggande autentisering i Go:

```Go
package main

import (
	"net/http"
	"fmt"
)

func main() {
	req, err := http.NewRequest("GET", "https://example.com", nil)
	
	if err != nil {
		fmt.Println(err)
		return
	}
	
	req.SetBasicAuth("username", "password")
	
	client := &http.Client{}
	resp, err := client.Do(req)
	
	if err != nil {
		fmt.Println(err)
		return
	}
	
	defer resp.Body.Close()
	fmt.Println("Response status:", resp.Status)
}
```
När du kör detta program kommer det att skicka en GET-förfrågan till `https://example.com` med användarnamnet `username` och lösenordet `password`.

## Lugn Dykning
Grundläggande HTTP-autentisering är ett enkelt, men kraftfullt sätt att skydda dina webbresurser. Ursprungligen definierad i HTTP/1.0-specifikationen, har det stått emot tidens prövning och används fortfarande flitigt som en enkel mekanism för autentisering.

Men det finns alternativ till grundläggande autentisering. Bearer Token autentisering, OAuth och Digest access autentisering är några av de andra populära metoderna.

När du implementerar grundläggande autentisering i Go, är det viktigt att ha SSL/TLS i åtanke. Eftersom grundläggande autentisering skickar lösenord i klartext, bör du alltid använda HTTPS när du använder grundläggande autentisering för att förhindra att lösenorden sniffas av illvilliga aktörer.

## Se Även
Här är några länkar till ytterligare resurser:
1. [Go Docs: http package](https://golang.org/pkg/net/http/)
2. [HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
3. [Go by Example: HTTP Clients](https://gobyexample.com/http-clients)