---
title:                "Å sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Clojure: Å sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Å sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hvorfor
HTTP-forespørsler med grunnleggende autentisering kan være nyttig når du trenger å sende sensitiv informasjon til en server, som for eksempel et API-kall. Dette sikrer at kun autoriserte brukere har tilgang til dataene, og beskytter sensitiv informasjon fra uautorisert tilgang.

## Hvordan
For å sende en HTTP-forespørsel med grunnleggende autentisering i Clojure, kan du følge disse trinnene:

Først må du importere `clj-http.core`, som gir funksjoner for å sende HTTP-forespørsler og behandle svarene. Dette gjøres ved å legge til følgende linje i toppen av koden din:

```Clojure
(:require [clj-http.client :as client])
```

Deretter må du definere URL-adressen du vil sende forespørselen til, og autentiseringsinformasjonen du vil bruke. Dette kan gjøres ved å opprette en `auth`-mappe med nøklene "username" og "password":

```Clojure
(def url "https://example.com/api")
(def auth {:username "brukernavn" :password "passord"})
```

Til slutt kan du sende forespørselen ved å bruke `client/post`-funksjonen og angi autentiseringsinformasjonen som en valgfri parameter:

```Clojure
(client/post url {:auth auth})
```

Dette vil returnere en respons-struct med status, kropp og eventuelle headerverdier. Du kan så behandle svaret videre i koden din.

## Deep Dive
HTTP-forespørsler med grunnleggende autentisering sender autentiseringsinformasjon over en usikker forbindelse, derfor bør den bare brukes over HTTPS for å sikre at informasjonen ikke blir avlyttet. Det finnes også andre typer autentisering som er mer sikre, som for eksempel OAuth.

Dersom du trenger å sende tilpassede autentiseringshodere eller bruke en annen autentiseringsmetode, kan du bruke `client/request`-funksjonen som gir mer fleksibilitet. Den tar også imot en `auth`-parameter og lar deg definere en tilpasset fn-funksjon for å bygge opp en autentisering.

## Se også
- https://github.com/dakrone/clj-http - Offisiell dokumentasjon for `clj-http`-biblioteket
- https://blog.jayway.com/2014/07/04/rest-clj-an-idiomatic-clojure-http-client/ - En veiledning for å sende HTTP-forespørsler i Clojure