---
title:                "Å laste ned en nettside"
html_title:           "Gleam: Å laste ned en nettside"
simple_title:         "Å laste ned en nettside"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor laste ned en nettstedsside? Det kan være mange grunner - kanskje du trenger å analysere data fra siden, eller du ønsker å lagre informasjonen for senere bruk.

## Slik gjør du det

For å laste ned en nettstedsside i Gleam, kan du bruke funksjonen `download.binary()` i HTTP biblioteket. Denne funksjonen tar inn en URL som parameter og returnerer en `Result` som enten er en `Ok` med binærdata fra nedlastingen, eller en `Err` med en feilmelding.

```Gleam
import http

let result = http.download.binary("https://www.example.com")

case result {
    Ok(data) -> {
        // Gjør noe med den binære dataen
        // For eksempel lagre den til en fil
        File.put_contents("./site.html", data)
        Ok("Nettstedet ble lastet ned og lagret!")
    }
    Err(error) -> {
        // Håndter eventuelle feil
        Err(error)
    }
}
```

Output:

```
Nettstedet ble lastet ned og lagret!
```

## Dypdykk

Når du laster ned en nettstedsside, får du tilgang til all informasjonen som er på den siden. Dette kan være svært nyttig for å hente ut data eller holde en kopi av en side for fremtidig referanse. Du kan også bruke Gleam sine innebygde stringfunksjoner for å manipulere og analysere innholdet på siden.

## Se også

- [Gleam HTTP bibliotek](https://gleam.run/documentation/std-lib-http/)
- [Offisiell Gleam nettside](https://gleam.run/)