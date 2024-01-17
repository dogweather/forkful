---
title:                "Å sende en http-forespørsel"
html_title:           "Go: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Når man bruker Go programmeringsspråket, kan man enkelt sende en HTTP for å be om informasjon fra en nettside eller API. Dette lar programmerere hente og behandle data på en effektiv måte.

# Hvordan:
Go har innebygde funksjoner for å sende HTTP-forespørsler. Her er et eksempel på å sende en GET-forespørsel:

```Go
resp, err := http.Get("https://example.com")
if err != nil {
    // håndter feil
}
defer resp.Body.Close()
body, err := ioutil.ReadAll(resp.Body)
fmt.Println(string(body))
```

Output: HTML-koden fra nettsiden vil bli skrevet ut.

# Dykke dypere:
Å sende HTTP-forespørsler er en viktig del av å bygge webapplikasjoner og å samhandle med eksterne tjenester. Alternativene til å bruke Go for å gjøre dette inkluderer JavaScript for frontend eller Python for backend. Implementeringen av HTTP-forespørsler i Go bruker standardbiblioteker som net/http og io/ioutil.

# Se også:
- Offisiell Go dokumentasjon for å sende HTTP-forespørsler: https://golang.org/pkg/net/http/
- En guide for å bruke Go med RESTful API-er: https://www.thepolyglotdeveloper.com/2017/02/consume-restful-api-endpoints-golang-application/