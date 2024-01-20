---
title:                "Verkkosivun lataaminen"
html_title:           "C#: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

Web-sivun lataaminen tarkoittaa sen sisällön noutamista verkosta. Ohjelmoijat tekevät niin esimerkiksi web-sisällön analysointia tai datan keräämistä varten.

## Näin Se Tehtiin:

Clojure-lisäosassa `clj-http` on funktio `client/get` jota voimme käyttää. Asenna se Clojure-projektiisi lisäämällä `:dependencies` osioon `[clj-http "3.12.3"]`.

Koodi-osa näyttää tältä:

```clojure 
(require '[clj-http.client :as client])

(defn download-page [url]
  (let [response (client/get url)]
    (:body response)))
```

Kun ajat koodia näin: `(download-page "https://www.example.com")`, saat tulokseksi koko sivun HTML-koodin.

## Syvällinen Tutkimus:

Historiallisesti ottaen, web-sivujen lataaminen on ollut tärkeä osa web-skarppausta ja data-analytiikkaa. 

Alternatiivisesti, voit käyttää muita Clojure-kirjastoja tai API-rajapintoja lataamiseen. Esim. `clj-http` lisäosan sijasta voit käyttää `http-kit` lisäosaa. 

Lisätietoja pystyy löytämään `clj-http` dokumentaation [täältä](https://clj-http.org/).

## Katso Lisää:

- clj-http dokumentaatio: https://clj-http.org/
- HTTP-protokollasta: https://fi.wikipedia.org/wiki/HTTP
- Web-skarppauksesta ja data-analytiikasta: https://www.datacamp.com/community/tutorials/web-scraping-clojure