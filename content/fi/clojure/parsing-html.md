---
title:                "HTML:n jäsentäminen"
html_title:           "Clojure: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

HTML parsing tarkoittaa HTML-koodin purkamista ja analysointia. Ohjelmoijat käyttävät tätä työkalua lukeakseen ja muokatakseen verkkosivujen sisältöä ja rakennetta. 


## Kuinka tehdä:

```Clojure
(def html-string "<html><head><title>Otsikko</title></head><body><p>Tervetuloa!</p></body></html>")

(require '[clojure.data.xml :as xml])

(xml/parse-str html-string)
; => {:tag :html, :attrs nil, :content [{:tag :head, :attrs nil, :content [{:tag :title, :attrs nil, :content ["Otsikko"]}]} {tag :body, :attrs nil, :content [{:tag :p, :attrs nil, :content ["Tervetuloa!"]}]}]} 
```

## Syvemmälle sukeltaminen:

HTML-parsingilla on pitkä historia ja se on tärkeä työkalu verkkokehittäjille. On olemassa myös muita tapoja selata HTML-sisältöä, kuten käyttämällä CSS-selektoreita tai XPath-kyselykieltä. Clojuren tapauksessa käytetään clojure.data.xml -kirjastoa, joka perustuu .NET-ohjelmointikieleen.

## Katso myös:

https://clojure.github.io/clojure/clojure.data.xml-api.html