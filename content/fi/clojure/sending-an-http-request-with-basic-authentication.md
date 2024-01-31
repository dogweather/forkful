---
title:                "HTTP-pyynnön lähettäminen perusautentikoinnilla"
date:                  2024-01-20T18:01:26.386642-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP-pyynnön lähettäminen perusautentikoinnilla"

category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
HTTP-pyynnön lähettäminen perusautentikaatiolla tarkoittaa verkkoresurssin pyytämistä käyttäen käyttäjänimeä ja salasanaa. Ohjelmoijat tekevät tämän päästäkseen käsiksi suojattuihin dataresursseihin.

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
