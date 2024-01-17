---
title:                "Lähettäminen http-pyyntö"
html_title:           "Clojure: Lähettäminen http-pyyntö"
simple_title:         "Lähettäminen http-pyyntö"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Lähettäessäsi HTTP-pyynnön, lähetät tietoa toiselle tietokoneelle verkossa. Ohjelmoijat tekevät tämän saadakseen tietoa tai suorittaakseen jotain toisessa tietokoneessa sijaitsevassa sovelluksessa.

## Miten:

```Clojure 
(require '[clj-http.client :as http]) 
(def response (http/get "https://example.com")) 
(:body response)
```

Tämän koodin suorittamisen jälkeen saat vastauksena pyytämäsi sivun html-koodin. Voit lähettää myös muita HTTP-pyyntöjä, kuten POST, PUT ja DELETE.

## Syvemmälle:

Historiallisessa kontekstissa HTTP-pyynnöillä on ollut tärkeä rooli kehityksessä verkkosivujen tietojen käsittelyssä. On myös muita tapoja kommunikoida sovellusten välillä, kuten WebSockets tai RPC, mutta HTTP on yksi yleisimmistä ja yksinkertaisimmista tavoista lähettää tietoa verkossa.

## Katso myös:

- [Clojure-kirjasto HTTP-pyyntöjen lähettämiseen](https://github.com/dakrone/clj-http)
- [HTTP-protokollan perusteet](https://developer.mozilla.org/fi/docs/Web/HTTP/Overview)