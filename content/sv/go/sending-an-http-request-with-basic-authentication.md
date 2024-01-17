---
title:                "Skicka en http-begäran med grundläggande autentisering"
html_title:           "Go: Skicka en http-begäran med grundläggande autentisering"
simple_title:         "Skicka en http-begäran med grundläggande autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Att skicka en HTTP-förfrågan med grundläggande autentisering innebär att man skickar en begäran till en webbserver med ett användarnamn och lösenord som autentiseringsuppgifter. Programerare gör detta för att säkra sin kommunikation med webbservern och kontrollera åtkomst till skyddade resurser.

## Så här gör du:

```Go
// Importera nödvändiga paket
import (
   "fmt"
   "net/http"
)

// Ange autentisieringsuppgifter
username := "användarnamn"
password := "lösenord"

// Skapa en HTTP-klient
client := &http.Client{}

// Skapa en förfrågan med grundläggande autentisering
req, err := http.NewRequest("GET", "https://www.example.com", nil)
req.SetBasicAuth(username, password)

// Skicka förfrågan och få svar
resp, err := client.Do(req)
defer resp.Body.Close()

// Hantera svar
if err != nil {
   panic(err)
}
fmt.Println("Statuskod:", resp.Status)

```

Output:
```
Statuskod: 200 OK
```

## Djupdykning:

* **Historisk kontext:** Grundläggande autentisering är en av de äldsta metoderna för autentisering på webben. Det infördes redan 1995 som en del av HTTP-protokollet och har sedan dess använts som en grundläggande säkerhetsmekanism för webbtjänster.

* **Alternativ:** Utöver grundläggande autentisering finns det flera andra autentiseringsprotokoll som OAuth och JWT (JSON Web Token) som är mer säkra och flexibla.

* **Implementeringsdetaljer:** HTTP-förfrågan med grundläggande autentisering skapar en HTTP-beteckning som har en autentiseringssektion i dess HTTP-autoriseringsrubrik, vilken innehåller användarnamn och lösenord. Dessa uppgifter skickas sedan med i varje begäran till webbservern för autentisering.

## Se även:

* [Go GoDocs: "net/http" paketet](https://godoc.org/net/http)
* [HTTP grundläggande autentisering specification](https://developer.mozilla.org/sv/docs/Web/HTTP/Headers/Authorization)