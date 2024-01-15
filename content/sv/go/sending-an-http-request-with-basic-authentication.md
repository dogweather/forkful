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

## Varför

Om du vill skicka en säker HTTP-förfrågan till en webbtjänst, behöver du använda en autentiseringsmetod för att verifiera din identitet. En av de enklaste metoderna är Basic Authentication, som kräver användarnamn och lösenord för att godkänna åtkomst. Genom att lära dig hur man skickar en HTTP-förfrågan med Basic Authentication i Go, kan du bygga säkra och pålitliga webbapplikationer.

## Hur man gör

För att skicka en HTTP-förfrågan med Basic Authentication i Go, behöver du bara följa några enkla steg.

Först behöver du importera paketet "net/http" och "encoding/base64".

```Go
import (
    "net/http"
    "encoding/base64"
)
```

Sedan måste du skapa en instans av typen "http.Client" och ange autentiseringsuppgifterna i en "http.Header" med hjälp av "SetBasicAuth" funktionen. Dessa autentiseringsuppgifter krypteras sedan med bas64-kodning.

```Go
client := &http.Client{}
req, _ := http.NewRequest("GET", "https://www.example.com/api", nil)
req.Header.Set("Authorization", "Basic " + base64.StdEncoding.EncodeToString([]byte("användarnamn:lösenord")))
```

Slutligen kan du genomföra din HTTP-förfrågan med hjälp av "Do" funktionen på din Client instans och utföra eventuella åtgärder med responsen.

```Go
resp, err := client.Do(req)
if err != nil {
    // Hantera fel
}
defer resp.Body.Close()

// Gör något med responsen, som att skriva ut den till konsolen
fmt.Println(resp.Status)
```

Output:

```
200 OK
```

## Deep Dive

En HTTP-förfrågan med Basic Authentication består av två delar - användarnamn och lösenord - som krypteras med bas64-kodning och skickas som en "Authorization" header. Detta gör att servern kan verifiera användarens identitet och ge åtkomst till resurser på ett säkert sätt.

När autentiseringsuppgifterna mottas på servern, dekrypteras de med hjälp av bas64-dekoder och jämförs med de som finns sparade på servern. Om de matchar så godkänns åtkomsten, annars returneras ett felmeddelande.

Det är viktigt att komma ihåg att Basic Authentication inte är den mest säkra autentiseringsmetoden och bör endast användas om det är nödvändigt och resurserna inte är känsliga. Annars är det bättre att använda mer robusta metoder som OAuth eller JWT.

## Se även

- [Go Documentation - Package http](https://golang.org/pkg/net/http/)
- [Go Documentation - Package encoding/base64](https://golang.org/pkg/encoding/base64/)
- [Basic Authentication RFC](https://tools.ietf.org/html/rfc7617)