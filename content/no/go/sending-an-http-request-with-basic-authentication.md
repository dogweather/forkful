---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering er en prosess hvor en klient sender en forespørsel til en server og inkluderer autentiseringsdetaljer for å bevise sin identitet. Programmerere gjør dette for å sikre at sensitive data kun gis til validerte brukere.

## Hvordan gjør vi det:
Her er et enkelt eksempel på hvordan du sender en HTTP-forespørsel med grunnleggende autentisering i Go.

```Go
package main

import (
	"fmt"
	"net/http"
	"net/http/httputil"
)

func main() {
	req, err := http.NewRequest("GET", "http://eksempel.no", nil)
	req.SetBasicAuth("brukernavn", "passord")

	res, err := http.DefaultClient.Do(req)
	if err != nil {
		panic(err)
	}
	defer res.Body.Close()

	dump, _ := httputil.DumpResponse(res, true)
	fmt.Println(string(dump))
}
```

Kjør programmet og du vil se en komplett HTTP-respons dump direkte i terminalen.

## Dykk etter mer kunnskap
Historisk sett har grunnleggende autentisering vært brukt siden HTTP/1.0, en enkel måte å beskytte nettsider og data. Det viktigste alternative i dag er kanskje Bearer-token, oftest brukt med OAuth2. Ved implementering i Go, la merke til hvordan `SetBasicAuth`-metoden setter autentiseringsdetaljer i HTTP `Authorization` header.

## Se også
For mer detaljer om HTTP-autentisering med Go, sjekk ut disse ressursene:

1. Go Dokumentasjonen: [http pakken](https://golang.org/pkg/net/http/)
2. Blogginnlegg: [Basic Authentication in Go](https://www.alexedwards.net/blog/basic-authentication-in-go)
3. God praksis for autentisering: [OWASP Authentication Cheat Sheet](https://cheatsheetseries.owasp.org/cheatsheets/Authentication_Cheat_Sheet.html)