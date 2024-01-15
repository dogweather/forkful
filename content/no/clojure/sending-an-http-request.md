---
title:                "Sending en http-forespørsel"
html_title:           "Clojure: Sending en http-forespørsel"
simple_title:         "Sending en http-forespørsel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hvorfor 
Du lurer kanskje på hvorfor du skulle bry seg om å sende HTTP-forespørsler i Clojure. Vel, det er et veldig nyttig verktøy for å kommunisere med eksterne API-er eller hente data fra andre nettsteder. Det kan hjelpe deg med å bygge kraftige applikasjoner som er avhengige av å få og behandle informasjon fra ulike kilder.

## Hvordan 
Så, la oss se på hvordan du kan sende en HTTP-forespørsel ved hjelp av Clojure. I eksemplene nedenfor vil vi bruke [clj-http](https://github.com/dakrone/clj-http) biblioteket, som gir en enkel og intuitiv måte å sende HTTP-forespørsler og behandle svarene på.

Først må du inkludere biblioteket i prosjektet ditt ved å legge til følgende avhengighet i din `project.clj` fil:

```
[clj-http "3.11.0"]
```

Deretter importerer du biblioteket i Clojure-filen din:

```
(ns min-app.core
  (:require [clj-http.client :as client]))
```

For å sende en HTTP GET-forespørsel, bruker du `client/get` funksjonen og angir URL-en du vil sende forespørselen til:

```
(def res (client/get "https://www.example.com"))
```

Hvis forespørselen er vellykket, returnerer `client/get` funksjonen et map med følgende nøkler: `:status`, `:headers` og `:body`. Så vi kan for eksempel få tilgang til kroppen (body) av svaret ved å kalle `:body` nøkkelen på resultatmapet:

```
(:body res)
```

Du kan også inkludere eventuelle parametere eller tilpassede HTTP-headers i forespørselen ved å bruke `:query-params` og `:headers` argumenter i `client/get` funksjonen.

## Dypdykk 
Nå som du har lært det grunnleggende, la oss se på noen flere detaljer om å sende HTTP-forespørsler i Clojure.

For å sende en POST-forespørsel, bruker du `client/post` funksjonen og angir URL-en og eventuelt kroppen (body) til forespørselen din:

```
(def res (client/post "https://www.example.com" 
        {:body "data=123"}))
```

Du kan også bruke `client/post` funksjonen til å sende JSON-data ved å inkludere `content-type` header og legge til JSON-data i kroppen (body):

```
(def res (client/post "https://www.example.com" 
        {:json {:key "value"}} 
        {:headers {"content-type" "application/json"}}))
```

Du kan også sende HTTP-forespørsler ved hjelp av andre funksjoner som `client/put`, `client/patch` og `client/delete` avhengig av hvilken type forespørsel du trenger å sende.

## Se Også 
- [clj-http biblioteket](https://github.com/dakrone/clj-http)
- [Clojure HTTP-forespørsler: Hvorfor og hvordan](https://www.braveclojure.com/quests/http-requests/)