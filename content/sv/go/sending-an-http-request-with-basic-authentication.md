---
title:                "Skicka en HTTP-förfrågan med Basic-autentisering"
date:                  2024-01-20T18:01:36.953002-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skicka en HTTP-förfrågan med Basic-autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att sända en HTTP-förfrågan med grundläggande autentisering innebär att skicka användarnamn och lösenord i klartext, kodat i Base64, för att få tillgång till en skyddad resurs. Programmerare gör detta för att enkelt verifiera användaridentitet vid kommunikation med webbtjänster.

## Hur gör man:
```Go
package main

import (
	"encoding/base64"
	"fmt"
	"net/http"
)

func main() {
	client := &http.Client{}
	req, err := http.NewRequest("GET", "https://example.com/resource", nil)
	if err != nil {
		panic(err)
	}
	
	auth := base64.StdEncoding.EncodeToString([]byte("username:password"))
	req.Header.Add("Authorization", "Basic "+auth)
	
	resp, err := client.Do(req)
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	
	fmt.Println("Statuskod:", resp.StatusCode)
}
```
Exemplet skapar en HTTP-förfrågan, kodar autentiseringsuppgifter med Base64 och skickar förfrågan. Vid framgång skrivs statuskoden ut.

## Djupdykning
HTTP Basic Authentication har använts sedan HTTP/1.0 och är en av de enklaste formerna av autentisering via nätverk. Det ersätts ofta av säkrare metoder såsom OAuth för att skydda känslig information bättre. I Go sköter standardbiblioteket http mycket av det tunga arbetet. Viktigt att notera är att HTTPS bör användas för att kryptera autentiseringsuppgifter över nätverket, annars kan känslig data lätt bli komprometterad.

## Se Också
- Go's officiella dokumentation för `net/http` biblioteket: https://golang.org/pkg/net/http/
- En genomgång av autentiseringstyper i HTTP: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Information om HTTP-statuskoder: https://www.restapitutorial.com/httpstatuscodes.html
