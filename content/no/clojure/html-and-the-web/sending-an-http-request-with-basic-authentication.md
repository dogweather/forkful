---
title:                "Å sende en HTTP-forespørsel med grunnleggende autentisering"
date:                  2024-01-20T18:01:25.904701-07:00
model:                 gpt-4-1106-preview
simple_title:         "Å sende en HTTP-forespørsel med grunnleggende autentisering"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
HTTP-forespørsler med grunnleggende autentisering innebærer å sende brukernavn og passord i en header for tilgang. Programmerere bruker dette for å sikre endepunkter og begrense tilgangen til autoriserte brukere.

## Hvordan gjøre det:
For å sende en HTTP-forespørsel med grunnleggende autentisering i Clojure, kan vi bruke `clj-http` biblioteket. Her er et eksempel:

```Clojure
(require '[clj-http.client :as client])

(let [credentials (str "Basic " (.encode (java.util.Base64/getEncoder) (.getBytes "brukernavn:passord")))]
  (client/get "https://eksempel.com/ressurs"
              {:headers {"Authorization" credentials}}))
```

Eksempel på svar:

```Clojure
{:status 200
 :headers {"Content-Type" "application/json"}
 :body "{\"data\":\"verdi\"}"}
```

## Dypdykk
Grunnleggende autentisering har vært en enkel HTTP-autentiseringsmekanisme siden tidlig på internettet. Sikkerhetsrisikoen inkluderer at legitimasjon i klartekst kan avlyttes hvis ikke HTTPS brukes.

Alternativer til grunnleggende autentisering inkluderer OAuth, API-nøkler og JWT (JSON Web Tokens), som alle kan tilby en høyere sikkerhetsnivå.

Implementeringsdetaljer i Clojure kan kreve ekstra oppsett for HTTPS og håndtering av ulike autentiseringsflyter, basert på hvilke sikkerhetstiltak som er på plass på serveren du kommuniserer med.

## Se også

- `clj-http` dokumentasjon: [https://github.com/dakrone/clj-http](https://github.com/dakrone/clj-http)
- ClojureDocs, en Clojure-samfunnsdrevet dokumentasjonsressurs: [https://clojuredocs.org/](https://clojuredocs.org/)
- IETF HTTP Basic Authentication standard: [https://tools.ietf.org/html/rfc7617](https://tools.ietf.org/html/rfc7617)