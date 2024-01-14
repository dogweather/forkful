---
title:                "Clojure: Sending en http forespørsel"
simple_title:         "Sending en http forespørsel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor

HTTP-forespørsler er en viktig del av moderne programmering, spesielt i Clojure. Ved å sende HTTP-forespørsler kan du hente og sende data til og fra andre servere og nettjenester. Dette er nyttig for å integrere funksjonalitet fra forskjellige kilder i applikasjonene dine.

## Hvordan

For å sende en HTTP-forespørsel i Clojure, kan du bruke biblioteket `clj-http`. Først må du importere biblioteket i prosjektet ditt:
```Clojure
(ns min-prosjekt.core
  (:require [clj-http.client :as http]))
```

Deretter kan du bruke funksjonen `http/post` for å sende en POST-forespørsel til en URL:
```Clojure
(http/post "https://api.norsk-data.no/tall" {:form-params {"nummer" 123}})
```
I dette eksempelet sender vi en forespørsel til en nettjeneste som gir oss informasjon om et tall. Vi spesifiserer URL-en og dataene vi vil sende som en mappe med nøkler og verdier.

Du kan også bruke `http/get` for å sende en GET-forespørsel. Begge disse funksjonene returnerer et svartobjekt som du deretter kan manipulere og behandle.

## Dypdykk

Når du sender en HTTP-forespørsel, er det flere ting å være oppmerksom på. For det første må du sørge for at URL-en er riktig formatert, ellers vil forespørselen mislykkes. Det er også viktig å håndtere eventuelle feil som kan oppstå, for eksempel hvis serveren er nede eller om nettverksforbindelsen er dårlig.

Videre kan du også spesifisere forskjellige parametere i forespørselen, som headers og timeouts. Du kan også sende forespørsler asynkront ved hjelp av `http/async-get` og `http/async-post`.

## Se også
- Dokumentasjon for `clj-http`: https://github.com/dakrone/clj-http
- En veiledning til å håndtere HTTP-forespørsler i Clojure: https://clojure.org/guides/http_requests