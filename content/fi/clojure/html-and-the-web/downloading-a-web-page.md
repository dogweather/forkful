---
date: 2024-01-20 17:43:52.850732-07:00
description: "How to: - N\xE4in teet sen: Clojuren avulla voit ladata webbisivun sis\xE4\
  ll\xF6n k\xE4ytt\xE4m\xE4ll\xE4 `clj-http`-kirjastoa. T\xE4ss\xE4 yksinkertainen\
  \ esimerkki."
lastmod: '2024-04-05T22:38:56.783806-06:00'
model: gpt-4-1106-preview
summary: "- N\xE4in teet sen: Clojuren avulla voit ladata webbisivun sis\xE4ll\xF6\
  n k\xE4ytt\xE4m\xE4ll\xE4 `clj-http`-kirjastoa. T\xE4ss\xE4 yksinkertainen esimerkki."
title: Verkkosivun lataaminen
weight: 42
---

## How to: - Näin teet sen:
Clojuren avulla voit ladata webbisivun sisällön käyttämällä `clj-http`-kirjastoa. Tässä yksinkertainen esimerkki:

```clojure
(require '[clj-http.client :as client])

(defn download-page [url]
  (client/get url))

;; Käyttö esimerkki:
(println (download-page "http://example.com"))
```

Vastaanotettu tieto näyttää suunnilleen tältä:

```clojure
{:status 200,
 :headers {"Content-Type" "text/html; charset=UTF-8", ...},
 :body "<!doctype html>..."}
```

## Deep Dive - Syvä sukellus:
Alkujaan, web-sivujen lataaminen perustui pelkkiin HTTP GET -pyyntöihin. Nykyisin, dynaamiset sivut vaativat monimutkaisempia tekniikoita kuten AJAX. Clojuren `clj-http`-kirjasto yksinkertaistaa tämän prosessin. 

Vaihtoehtoisia työkaluja ovat esim. `http-kit` tai `Aleph`. Niitä voidaan käyttää monenlaisten HTTP-pyyntöjen tekemiseen ja ne tulevat erilaisilla suorituskykyetuja.

Kun lataat sivua, huomioi oikeudet ja rajoitukset - esimerkiksi `robots.txt` tiedosto voi rajoittaa skrapausta.

## See Also - Katso myös:
- clj-http GitHub: https://github.com/dakrone/clj-http
- http-kit kotisivu: http://www.http-kit.org/
- Aleph GitHub: https://github.com/ztellman/aleph
- Clojure virallinen sivu: https://clojure.org
- Web scraping ohjeet: https://www.dataquest.io/blog/web-scraping-tutorial-clojure/
