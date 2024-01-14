---
title:                "Clojure: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi: Miksi ladata web-sivu?

Ladatessa web-sivun voit tutkia sen sisältöä ja käyttää sitä hyödyksi eri tarkoituksiin. Voit esimerkiksi analysoida sisältöä tai käyttää sitä osana sovellusta.

## Miten: Esimerkki koodin avulla

```clojure
(ns lataa-sivu.core
  (:require [clojure.java.io :as io]
            [clj-http.client :as http]))

;; lataa sivu ja tallenna se muuttujaan "response"
(def response (http/get "https://www.example.com"))

;; tulostetaan muuttujan sisältö
(println (:body response))
```

Tulostus:
```
<p>Tämä on esimerkkisivu.</p>
```

## Syvempi sukellus

Web-sivun lataaminen Clojurella onnistuu helposti käyttäen clj-http-kirjastoa. Kirjasto tarjoaa erilaisia funktioita HTTP-pyyntöjen tekemiseen ja vastauksien käsittelyyn. Voit esimerkiksi määrittää lisäparametreja pyyntöön, kuten otsikoita tai käyttää erilaisia HTTP-verbejä kuten POST tai PUT.

## Katso myös
- [Clj-http-kirjaston dokumentaatio](https://github.com/dakrone/clj-http)
- [Clojuren virallinen verkkosivusto](https://clojure.org/)
- [Clojure-yhteisön foorumi](https://clojureverse.org/)