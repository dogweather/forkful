---
title:                "Sending en http-forespørsel med grunnleggende autentisering"
html_title:           "Clojure: Sending en http-forespørsel med grunnleggende autentisering"
simple_title:         "Sending en http-forespørsel med grunnleggende autentisering"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å sende en HTTP-forespørsel med grunnleggende autentisering betyr å legge til informasjon om brukernavn og passord i forespørselen for å få tilgang til beskyttede ressurser på en nettside. Dette gjøres vanligvis av programmerere for å sikre at bare autoriserte brukere får tilgang til visse deler av en nettside.

## Hvordan:
### Eksempel 1: Bruke basic-auth bibliotek
```Clojure
(require '[cemerick.friend.basic-auth :as ba])
;; Opprett en brukerliste
(def users {"admin" {:username "admin" :password "password"}})
;; Generer en autentiseringsfunksjon
(defn auth-fn [{:keys [username password]}]
  (some #(and (= username (:username %))
              (ba/check-pass password (:password %)))
                users))
;; Legg til autentiseringsfunksjonen til en middleware-kjede
(def handler (-> app (wrap-authentication auth-fn)))
```
### Eksempel 2: Bruke clj-http bibliotek
```Clojure
(require '[clj-http.client :as http])
;; Sett inn brukernavn og passord i en header
(http/request 
  {:url "http://eksempel.com/sikker" 
   :method :get 
   :headers {"Authorization" (str "Basic " (cred-str "username" "password"))}})
```
Eksempel 2 vil sende en GET-forespørsel til http://eksempel.com/sikker med brukernavn og passord i en HTTP-header.

## Dykk dypere:
### Historisk kontekst:
HTTP basic authentication ble introdusert i HTTP 1.0-standarden som en forenklet metode for autentisering. Det var den første formen for HTTP-autentisering, men har senere blitt erstattet av mer sikre metoder som token-basert autentisering.

### Alternativer:
I stedet for basic authentication, kan programmerere også bruke token-basert autentisering som OAuth eller OpenID Connect som gir en mer sikker og skalerbar måte å autentisere brukere.

### Implementeringsdetaljer:
HTTP basic authentication krever at brukernavn og passord blir kodet til Base64-før de blir sendt over nettverket. Det er viktig å merke seg at dette er ikke en krypteringsmetode, bare en form for kodetekst som kan dekodes av alle som får tilgang til forespørselen.

## Se også:
- [cemerick.friend.basic-auth](https://github.com/cemerick/friend/tree/master/src/cemerick/friend/basic_auth.clj)
- [clj-http](https://github.com/dakrone/clj-http)