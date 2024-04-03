---
date: 2024-01-20 18:01:26.386642-07:00
description: "Miten: Clojurella HTTP-pyynt\xF6ihin l\xE4hetet\xE4\xE4n usein k\xE4\
  ytt\xE4en `clj-http` kirjastoa. Alla on esimerkki perusautentikaation lis\xE4\xE4\
  misest\xE4."
lastmod: '2024-03-13T22:44:56.184283-06:00'
model: gpt-4-1106-preview
summary: "Clojurella HTTP-pyynt\xF6ihin l\xE4hetet\xE4\xE4n usein k\xE4ytt\xE4en `clj-http`\
  \ kirjastoa."
title: "HTTP-pyynn\xF6n l\xE4hett\xE4minen perusautentikoinnilla"
weight: 45
---

## Miten:
Clojurella HTTP-pyyntöihin lähetetään usein käyttäen `clj-http` kirjastoa. Alla on esimerkki perusautentikaation lisäämisestä.

```Clojure
(require '[clj-http.client :as client])

(defn fetch-protected-resource []
  (let [url "http://your-protected-resource.com"
        credentials {:auth {:user "kayttajanimi" :password "salasana"}}]
    (client/get url credentials)))

(println (fetch-protected-resource))
```
Tämä palauttaa palvelimelta saadun vastauksen.

## Syväsukellus
Perusautentikaatio on yksinkertainen autentikointimenetelmä HTTP-protokollassa. Se sisältyi jo RFC 2617 -standardiin vuonna 1999 ja on edelleen käytössä, vaikkakin vähemmän turvallisena kuin uudemmat menetelmät kuten OAuth. Vaihtoehtoina ovat muun muassa digitaalinen allekirjoitus ja token-autentikaatio. Clojuressa `clj-http` tekee autentikoinnin lisäämisestä yksinkertaista, mutta olennaista on muistaa käyttää HTTPS-yhteyttä, jotta tunnistetiedot pysyvät suojattuina.

## Katso Myös
- clj-http GitHub-sivu: https://github.com/dakrone/clj-http
- HTTP-autentikaatio: https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication
- Clojure virallinen dokumentaatio: https://clojure.org/guides/getting_started
