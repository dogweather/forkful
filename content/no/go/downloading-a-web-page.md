---
title:                "Go: Laste ned en nettside"
simple_title:         "Laste ned en nettside"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor?

Å laste ned en nettside kan være nyttig for en rekke formål, som for eksempel å utføre web scraping for å samle data, teste nettsidens ytelse, eller enkelt og greit lagre nettsiden offline for senere bruk.

## Hvordan gjøre det

For å laste ned en nettside i Go, må vi først importere pakken "net/http" og "io/ioutil". Deretter bruker vi funksjonen "http.Get()" til å hente nettsiden ved å gi den URL-en vi ønsker å laste ned som argument. Deretter bruker vi funksjonen "ioutil.WriteFile()" til å lagre nettsiden som en fil på vår lokale maskin.

```Go
import (
    "net/http"
    "io/ioutil"
)

resp, err := http.Get("https://www.example.com")
if err != nil {
    // håndter feil
}

defer resp.Body.Close()

body, err := ioutil.ReadAll(resp.Body)
if err != nil {
    // håndter feil
}

err = ioutil.WriteFile("example.html", body, 0644)
if err != nil {
    // håndter feil
}
```

Dette eksempelet bruker også "defer" til å sørge for at vi lukker "resp.Body" uansett om det oppstår en feil eller ikke. Vi bruker også "ioutil.ReadAll()" for å lese hele nettsiden og lagre innholdet som en bytestring.

## Deep Dive

Når vi bruker funksjonen "http.Get()", sender vi en HTTP-anmodning til nettsiden og mottar en HTTP-respons tilbake. Dette kan gi oss mer informasjon om nettsiden, som for eksempel dens statuskode og eventuelle headere. Vi kan også sende med spesifikke header-verdier, som brukernavn og passord, i anmodningen.

Vi kan også bruke pakken "net/http" for å lage og sende mer komplekse HTTP-anmodninger, som POST- og PUT-anmodninger. Dette kan være nyttig for å interagere med nettsider som krever pålogging eller som har et API vi ønsker å kommunisere med.

## Se også

- Offisiell Go-nettside: https://golang.org/
- Go-dokumentasjon for "net/http": https://golang.org/pkg/net/http/
- Go-dokumentasjon for "io/ioutil": https://golang.org/pkg/io/ioutil/