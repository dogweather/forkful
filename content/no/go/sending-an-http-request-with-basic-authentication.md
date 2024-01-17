---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Go: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Sending av en HTTP forespørsel med grunnleggende autentisering er en måte for programvareutviklere å sende og motta data fra en server med brukernavn og passord. Dette er nyttig for å sikre at bare autoriserte brukere har tilgang til visse ressurser på serveren.

## Hvordan:
I Go, kan du enkelt sende en HTTP forespørsel med grunnleggende autentisering ved å bruke standardpakken `net/http`. Følgende kodeblokk viser et eksempel på hvordan du kan gjøre dette:

```Go
// Importer net/http pakken
import "net/http"

// Lag en HTTP forespørselsobjekt
req, err := http.NewRequest("GET", "https://example.com/api/data", nil)

// Legg til brukernavn og passord i forespørselen
req.SetBasicAuth("brukernavn", "passord")

// Send forespørselen og håndter eventuelle feil
resp, err := http.DefaultClient.Do(req)
if err != nil {
    panic(err)
}

// Les responsen fra serveren
body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    panic(err)
}

// Skriv ut responsen
fmt.Println(string(body))
```

Eksempelutgangen vil vise dataene som ble mottatt fra serveren basert på den angitte URL-en og autentiseringsinformasjonen.

## Dypdykk:
HTTP-forespørsler med grunnleggende autentisering har vært en vanlig praksis for å sikre nettverkskommunikasjon siden HTTP-protokollen ble laget på 1990-tallet. Alternativene til grunnleggende autentisering inkluderer OAuth, Token Authentication og Digest Authentication. Disse alternative metodene er mer sikre enn grunnleggende autentisering, men krever vanligvis mer arbeid å implementere. I Go, kan du bruke tredjeparts biblioteker for å implementere disse alternative autentiseringsmetodene.

## Se også:
- [Pakkenet/http Dokumentasjon](https://golang.org/pkg/net/http/)
- [HTTP Basic Authentication på 2 minutter](https://youtu.be/_eUITgQbBVA) video fra Gopher Academy