---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/parsing-html.md"
---

{{< edit_this_page >}}

---

## Mitä & Miksi?

HTML:n jäsentäminen tarkoittaa HTML-dokumentin rakenteen muuttamista ymmärrettävään muotoon. Ohjelmoijat tekevät sen, koska se mahdollistaa sivujen sisällön analysoinnin ja muokkaamisen sujuvasti.

## Miten:

Clojuren `hiccup`-kirjastoa voidaan käyttää HTML:n jäsennykseen. 

```clojure
(ns my-namespace.core
  (:require [hiccup.core :refer [html]]))

(defn greet []
  (html
    [:html
      [:head
        [:title "Tervetuloa!"]]
      [:body
        [:h1 "Hei, maailma!"]]]))
```

Koodin suorittaminen palauttaa HTML-stringin:

```clojure
"<html><head><title>Tervetuloa!</title></head><body><h1>Hei, maailma!</h1></body></html>"
```

## Sukellus syvemmälle:

HTML-jäsennys sai alkunsa 1990-luvun alussa WWW:n kanssa. Ensimmäiset parserit olivat yksinkertaisia kirjastoja, mutta ovat sittemmin kehittyneet monimutkaisiksi työkaluiksi.

Clojure-kielessä on myös muita tapoja jäsennellä HTML-sisältöä, kuten `enlive` ja `clojure.data.xml`. Valinta riippuu siitä, kuinka kompleksisia sivustoja analysoit.

Hiccupin sisällä HTML-elementit esitetään Clojure-vetoina, joissa ensimmäinen elementti on tageja ja loput attribuutteja tai lapsielementtejä. Luonteenomaisen datarakenteen ansiosta hiccup mahdollistaa Clojure-syntaksin käytön HTML:n manipuloimiseksi.

## Katso myös:

1. Hiccupin dokumentaatio: [https://github.com/weavejester/hiccup](https://github.com/weavejester/hiccup)
2. Clojuren HTML-jäsentäminen – Stack Overflow: [https://stackoverflow.com/questions/3478592/parsing-html-in-clojure](https://stackoverflow.com/questions/3478592/parsing-html-in-clojure)

---

Huomio: Tämä artikkeli olettaa, että sinulla on perustiedot Clojuresta ja HTML:stä. Jos haluat syvällisempää tietoa, suosittelemme lukemaan lähdemateriaalia ja kokeilemaan koodiesimerkkejä.