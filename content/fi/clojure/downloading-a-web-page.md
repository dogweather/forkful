---
title:                "Verkkosivun lataaminen"
html_title:           "Clojure: Verkkosivun lataaminen"
simple_title:         "Verkkosivun lataaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Miksi 

Internetissä on paljon tietoa, mutta joskus haluat tallentaa jonkin sivun omalle tietokoneellesi tai käyttää sivun sisältöä ohjelmassasi. Tämä artikkeli näyttää, kuinka voit ladata verkkosivun käyttämällä Clojurea.

## Miten

Lataa verkkosivu ja tallenna se merkkijonona:

```Clojure
(def url "https://www.example.com/")
(def sivu (slurp url))
```

Lataa ja tallenna verkkosivu modelAndView-objektiin:

```Clojure
(defn lataa [url]
   (try
     (doto (modelAndView (new URL url))
       (.addObject "sivu" (.toString (slurp url))))
     (catch Exception e (modelAndView "error" "Jokin meni pieleen..."))))
```

Lataa ja tulosta verkkosivun sisältö:

```Clojure
(defn tulosta [url]
  (println (slurp url)))
```

Voit myös ladata ja tallentaa vain tietyt osat sivusta käyttämällä esimerkiksi "select"-funktiota ja CSS-sääntöjä.

```Clojure
(defn lataa-title [url]
  (->> url
       slurp
       (select [:title])
       first
       :content
       first))

(def title (lataa-title "https://www.example.com/"))
```

## Syvä sukellus

Clojuren "slurp"-funktio käyttää Javaa ja sen "URLConnection"-luokkaa ladatakseen verkkosivun. Jos sivun lataaminen epäonnistuu, se palauttaa virheen ("error"). Clojuren "select"-funktio käyttää puolestaan "jsoup"-kirjastoa XML/HTML-analysointiin. Voit myös käyttää muita kirjastoja, kuten "clj-http" tai "http-kit", ladataksesi verkkosivuja.

## Katso myös

- https://clojure.org/
- https://www.w3schools.com/cssref/default.asp
- https://github.com/clojure/clojure/blob/master/src/clj/clojure/core.clj
- https://github.com/jafingerhut/jafingerhut.github.com/blob/master/demos/pure_css/flexgrid.css