---
title:                "Sende en http-forespørsel med grunnleggende autentisering"
html_title:           "Kotlin: Sende en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sende en http-forespørsel med grunnleggende autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å sende en HTTP-forespørsel med grunnleggende autentisering innebærer å sende brukernavn og passord som en del av HTTP-overskriften for å få tilgang til beskyttede data. Dette gjøres for å sørge for sikker kommunikasjon mellom klient og server.

## Hvordan Gjør Man Det:

Clojure gjør oppgaven enkel ved å bruke `clj-http` biblioteket. Her er en enkel kodeeksempel for å sende en GET forespørsel med grunnleggende autentisering.

```Clojure
(require '[clj-http.client :as client])

(let [response (client/get "http://example.com" {:basic-auth ["username" "password"]})]
  (println (:status response)))
```
Etter at forespørselen er sendt, vil statuskoden bli skrevet ut.

## Dypdykk:

Historisk sett har HTTP-forespørsler med grunnleggende autentisering vært brukt siden webens tidlige dager som en enkel måte å beskytte nettressurser på.

En populær alternativ til grunnleggende autentisering er Bearer Token autentisering, som er mer sikker og fleksibel. For å implementere det i Clojure, kan du simpelthen erstatte `:basic-auth` med `:oauth-token` i koden ovenfor.

Når det kommer til implementeringsdetaljer, konverterer `clj-http` brukernavnet og passordet til en Base64-encodet string og inkluderer den i 'Authorization'-overskriften i HTTP-forespørselen.

## Se Også:

For mer informasjon, sjekk ut disse nyttige ressursene:
- `clj-http` GitHub Repo: https://github.com/dakrone/clj-http
- Official Clojure Docs: https://clojure.org/reference/reader
- Basic Authentication on Wikipedia: https://en.wikipedia.org/wiki/Basic_access_authentication
- Bearer Token Authentication on Wikipedia: https://en.wikipedia.org/wiki/Bearer_token