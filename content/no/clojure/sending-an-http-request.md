---
title:                "Å sende en http-forespørsel"
html_title:           "C++: Å sende en http-forespørsel"
simple_title:         "Å sende en http-forespørsel"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
En HTTP-forespørsel er en måte for et program å spørre en server om data. Programmerere bruker det for å hente eller sende data over internett.

## Hvordan:
Her er et enkelt eksempel på hvordan sende en GET HTTP-forespørsel ved hjelp av ```http-kit``` bibliotekk i Clojure:

```Clojure
(ns http-example.core
  (:require [org.httpkit.client :as http]))

(defn get-request []
  (let [response @(http/get "https://httpbin.org/get")]
    (println (:status response))
    (println (:headers response))
    (println (:body response))))
```
Når du kjører ```get-request``` funksjonen, vil du se noe slik:

```Clojure
200
{"Date" "Tue, 14 Sep 2021 20:00:00 GMT", "Content-Type" "application/json"...}
"{args: {}, headers: {host: "httpbin.org",.."
```

## Deep Dive
HTTP-forespørsler startet med opprettelsen av Hypertext Transfer Protocol (HTTP) i 1991. I Clojure, er det flere bibliotekker du kan bruke for å sende HTTP forespørsler, for eksempel ```http-kit```, ```clj-http``` og ```aleph```. Sett bort i fra forskjellige funksjonaliteter, er implementeringsdetaljene ganske like - det handler om å lage en forbindelse med serveren, sende en forespørsel og deretter vente på svaret.

## Se også
[http-kit GitHub](https://github.com/http-kit/http-kit)

[clj-http GitHub](https://github.com/dakrone/clj-http)

[aleph GitHub](https://github.com/ztellman/aleph)