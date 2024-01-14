---
title:                "Clojure: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
Hvorfor skulle noen ønske å sende en HTTP request med basic authentication? En av de vanligste årsakene er for å sikre at bare autoriserte brukere har tilgang til en ressurs eller informasjon.

## Hvordan å gjøre det
For å sende en HTTP request med basic authentication i Clojure, kan du bruke biblioteket "clj-http". Først må du importere biblioteket:

```Clojure
(ns min-prosjekt.core
  (:require [clj-http.client :as client]))
```

Deretter kan du lage en request med basic authentication ved å bruke funksjonen "basic-auth":

```Clojure
(client/basic-auth {:url "https://eksempel.com/api/ressurs"
                    :username "brukernavn"
                    :password "passord"})
```

Dette vil returnere en respons av typen "clj-http.client.response/Response", som inneholder informasjonen fra ressursen.

## Dypdykk
Når du sender en HTTP request med basic authentication, legges brukernavnet og passordet til på en "Authorization" header i requesten. Det er viktig å merke seg at denne informasjonen ikke er kryptert, så det er viktig å bruke HTTPS i stedet for HTTP for å sikre at informasjonen ikke blir påvirket av uautorisert tilgang.

En annen ting å være oppmerksom på er at i tilfelle ugyldig eller manglende autentisering, vil responsen være en feilkode (for eksempel 401 eller 403).

## Se også
- https://github.com/dakrone/clj-http
- https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- https://www.baeldung.com/clojure-http-client