---
title:                "Clojure: Lähettäminen http-pyynnön"
simple_title:         "Lähettäminen http-pyynnön"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Miksi

HTTP-pyyntöjen lähettäminen on tärkeää monissa nykyaikaisissa sovelluskehityksessä, kuten verkkosivujen toteuttamisessa tai API-palveluiden käyttämisessä. Se mahdollistaa tiedon välittämisen palvelimelta asiakkaalle ja takaisin päin.

## Kuinka

HTTP-pyyntöjen lähettäminen Clojurella on helppoa käyttämällä kirjastoa nimeltä clj-http. Ensiksi, kirjasto on lisättävä projektiin käyttämällä Leiningenia tai Clojure CLI:tä. Seuraavaksi, me voimme määrittää pyynnön URL-osoitteen, sekä mahdolliset parametrit tai otsikot, kuten alla näkyvässä esimerkissä.

```Clojure
(ns http-esimerkki
  (:require [clj-http.client :as httpClient]))

(defn tee-http-pyynto []
  (let [pyynto (httpClient/get "https://example.com" {:query-params {:name "John"}})]
    (println (:status pyynto))
    (println (:body pyynto))))

(tee-http-pyynto)
```

Tämän esimerkin tulos olisi: 200 ja "<!DOCTYPE html><html>...</html>".

## Syvällinen sukellus

HTTP-pyynnöt toimivat perinteisesti seuraavassa järjestyksessä: asiakas lähettää pyynnön palvelimelle, palvelin vastaa pyyntöön ja asiakas saa vastauksen. Pyynnön lähettämiseen liittyvät asiat, kuten parametrien määrittäminen ja otsikoiden lisääminen, ovat tärkeitä yksityiskohtia joita täytyy huomioida. Tärkeää on myös ymmärtää eri HTTP-metodeja, kuten GET, POST, PUT ja DELETE, ja milloin niitä tulisi käyttää.

## Katso myös

- [HTTP-pyyntöjen lähettäminen Clojurella](https://github.com/dakrone/clj-http)
- [Clojure Web Development](https://clojure.org/guides/web_development)
- [RESTful API -palveluiden käyttö Clojurella](https://www.graalvm.org/reference-manual/graal-isolate/info.html)