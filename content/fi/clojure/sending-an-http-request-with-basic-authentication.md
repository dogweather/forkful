---
title:                "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
html_title:           "Kotlin: Lähettäminen http-pyyntö perusautentikoinnin kanssa"
simple_title:         "Lähettäminen http-pyyntö perusautentikoinnin kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Lähettäminen HTTP-pyyntö perustodentamisella tarkoittaa, että lähetämme verkkopyynnön, joka sisältää tunnistetiedot salasanana ja käyttäjänimenä. Ohjelmoijat tekevät tämän päästäkseen käsiksi suojattuihin tietoihin tai resursseihin.

## Kuinka:

Aloitetaan asentamalla http-kit kirjasto. Leiningen-riippuvuus näyttää tältä:

```clojure
[http-kit "2.5.3"]
```

Työskennellään `client`-moduulilla, jonka avulla voimme luoda perustodentamisen HTTP-pyynnön:

```clojure
(ns your-namespace-here
  (:require [org.httpkit.client :as client]))

(defn basic-auth [username password]
  (let [creds (str username ":" password)]
    (str "Basic " (java.util.Base64/encoder (byte-array (.getBytes creds))))))
    
(defn get-with-auth [url username password]
  (client/get url {:basic-auth (basic-auth username password)}))

(defn -main [& args]
  (let [response (get-with-auth "http://your-url-here" "username" "password")]
    (println (:status response)
             (:headers response)
             (slurp (:body response)))))

(-main)
```

Näytteen tuloste voi näyttää tältä:

```clojure
200
{"Content-Type" "application/json; charset=UTF-8"...}
{"data": {...}}
```

## Syvempi sukellus

Perustodentaminen HTTP:n kanssa datan lähettämiseen tuli käyttöön rajoitettujen resurssien suojaamiseen. Se on jo vanhentunut, altis hyökkäyksille, ja se on vaihtunut turvallisempiin metodeihin, kuten OAuth, joka on yleisesti käytössä käyttäjätietojen vaihtamiseen verkkosovellusten välillä.

Natiivissa Clojuressa ei ole sisäänrakennettua kirjastoa tähän tehtävään, joten käytämme http-kit kirjastoa, mikä on alun perin kehitetty Clojuren valintaan HTTP:n käsittelyyn. Se on helppokäyttöinen ja tarjoaa monia ominaisuuksia, kuten client- ja server-moduulit, lomakkeiden käsittely, cookie- ja istuntotuki sekä WebSocket-tuki.

## Katso myös

1. Clojure - [virallinen sivu](https://clojure.org/)
2. http-kit - [GitHub-sivu](https://github.com/http-kit/http-kit)
3. Perustodentaminen - [Wikipedia](https://fi.wikipedia.org/wiki/Perustodentaminen)
4. OAuth - [Wikipedia](https://fi.wikipedia.org/wiki/OAuth)