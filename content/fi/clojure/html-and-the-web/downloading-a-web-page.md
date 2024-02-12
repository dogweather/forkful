---
title:                "Verkkosivun lataaminen"
aliases:
- /fi/clojure/downloading-a-web-page/
date:                  2024-01-20T17:43:52.850732-07:00
model:                 gpt-4-1106-preview
simple_title:         "Verkkosivun lataaminen"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? - Mitä & Miksi?
Ladataan nettisivu tarkoittaa sivun sisällön tallentamista paikallisesti. Ohjelmoijat tekevät sitä datan analysointia, skrapausta tai varmuuskopiointia varten.

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
