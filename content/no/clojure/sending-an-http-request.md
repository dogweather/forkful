---
title:                "Å sende en http-forespørsel"
html_title:           "Clojure: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?

Sending av HTTP forespørsler er en vanlig praksis i nettutvikling. Det lar programmerere kommunisere og hente data fra forskjellige nettsider og API-er. Dette gjør at man kan bygge interaktive og dynamiske applikasjoner som er effektive når det kommer til å hente og behandle informasjon fra ulike kilder.

# Hvordan:

```Clojure
;; Enkel HTTP forespørsel
(require '[clojure.http.client :as http])

(http/get "https://www.example.com")
```

```Clojure
;; HTTP forespørsel med tilleggsparametere og avansert håndtering av respons
(require '[clojure.http.client :as http])
(require '[clojure.data.json :as json])

;; Oppretter en map med tilleggsparametere
(def params {:headers {"Content-Type" "application/json"}})

;; Utfører en POST forespørsel og konverterer respons til en map
(def response (http/post "https://www.example.com" {"key" "value"} params))

;; Henter ut og konverterer responsens body til en map
(def result (json/read-str (:body response)))

;; Håndterer resultatet fra forespørselen
(println (:key result))
```

# Dypdykk:

Sending av HTTP forespørsler har vært en viktig del av nettutvikling siden starten av internett. Det finnes diverse alternativer til å sende HTTP forespørsler, som for eksempel biblioteket cURL. Clojure har også ulike metoder for å håndtere HTTP forespørsler, inkludert den felles vanen å bruke funksjonen `GET` eller `POST` fra biblioteket `clojure.http.client`. Denne funksjonen inneholder også en rekke nyttige tilleggsparametere som kan justeres etter behov.

# Se også:

- Clojure HTTP client dokumentasjon: https://github.com/dakrone/clj-http