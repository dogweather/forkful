---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Go: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# Hvorfor

Hvorfor skulle noen ønske å sende en HTTP-forespørsel med grunnleggende autentisering? Kanskje du ønsker å lage et enkelt autentiseringssystem for din applikasjon, eller du trenger å sikre at bare autoriserte brukere har tilgang til bestemte ressurser på en ekstern server. Uansett motivasjon, vil dette artikkelen vise deg hvordan du enkelt kan implementere grunnleggende autentisering i ditt Go-program.

# Hvordan

For å sende en HTTP-forespørsel med grunnleggende autentisering i Go, må du først importere "net/http" pakken. Deretter kan du bruke "http.NewRequest ()" -funksjonen for å lage en ny forespørsel. Du må spesifisere forespørselsmetoden, URL-en og eventuelle data som skal sendes med forespørselen, som i eksempelet nedenfor:

```Go
import (
    "net/http"
)

func sendRequest() {
    // Opprett en ny forespørsel
    req, err := http.NewRequest("GET", "https://eksempel.com/api/data", nil)
    if err != nil {
        // Behandle eventuelle feil
    }

    // Legg til autentiseringsinformasjonen i headeren
    req.SetBasicAuth("brukernavn", "passord")

    // Utfør forespørselen
    res, err := http.DefaultClient.Do(req)
    if err != nil {
        // Behandle eventuelle feil
    }

    // Les og behandle responsen
    body, err := ioutil.ReadAll(res.Body)
    if err != nil {
        // Behandle eventuelle feil
    }

    // Lukk responsen
    defer res.Body.Close()

    // Skriv ut responsen
    fmt.Println(string(body))
}
```

I eksempelet over bruker vi "http.DefaultClient.Do ()" -funksjonen for å utføre forespørselen. Vi bruker også "req.SetBasicAuth ()" for å legge til brukernavn og passord i headeren til enhver forespørsel som sendes fra vår klient.

# Deep Dive

Når du sender en HTTP-forespørsel med grunnleggende autentisering, må du legge til autentiseringsinformasjonen i headeren på følgende måte:

```Go
req.SetBasicAuth("brukernavn", "passord")
```

Dette legger til en "Authorization" -header i forespørselen som inneholder brukernavn og passord kodet i Base64. Det er viktig å merke seg at dette er en svært enkel autentiseringsmetode og ikke bør brukes for sensitive data.

I tillegg til å legge til autentiseringsinformasjonen i headeren, kan du også sjekke responskoden for å se om forespørselen ble utført vellykket. En HTTP-forespørsel med grunnleggende autentisering vil vanligvis returnere en responskode "200 OK" hvis autentiseringen var vellykket, eller "401 Unauthorized" hvis det var et problem med autentiseringen.

# Se Også

- [net/http pakken](https://golang.org/pkg/net/http/)
- [HTTP-forespørsler i Go](https://blog.golang.org/making-and-using-http-clients)
- [Grunnleggende autentisering](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication#Basic_authentication_scheme)