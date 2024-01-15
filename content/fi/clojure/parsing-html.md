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

## Miksi

Jos haluat analysoida verkkosivujen sisältöä ja hakea tiettyä tietoa tekstistä tai muotoiluista, HTML-parsiminen on välttämätöntä. Se mahdollistaa verkkosivujen tietojen keräämisen ja jatkoprosessoinnin helposti ja vaivattomasti.

## Kuinka tehdä

```Clojure
(require '[clojure.data.zip.xml :as xml])
(require '[clojure.xml :as xml-parse])

(def html (xml-parse "<html><body><h1>Otsikko</h1><p>Tekstiä</p></body></html>"))

(defn get-text [node]
  (when (xml/tag node)
    (apply str (map #(get-text %) (xml/children node))))
  (when (xml/text node)
    (xml/text node)))

(xml-> html :body :p get-text)
;; Tuloste: "Tekstiä"
```

## Syventyminen

HTML-parsiminen hyödyntää Clojuren sisäistä XML-paketointia, joka helpottaa HTML-elementtien käsittelyä. XML-sivutus toimii siten, että se navigoi XML-solmujen läpi ja tarjoaa pääsyn atribuutteihin ja teksteihin. Lisäksi Clojure tarjoaa monia hyödyllisiä kirjastoja, kuten clojure.data.zip.xml ja clojure.xml, jotka auttavat yksinkertaistamaan parsimista ja haunopeuttamista.

## Katso myös
- [Clojure XML-pakkauksen dokumentaatio](https://clojure.github.io/clojure/clojure.data.zip-api.html#clojure.data.zip.xml)
- [Clojure XML-pakkauksen lähdekoodi GitHubissa](https://github.com/clojure/clojure/blob/master/src/clj/clojure/data/zip/xml.clj)