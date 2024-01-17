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

## Miksi ja mitä?
Lataaminen web-sivulta tarkoittaa tiedon hakemista internetistä ja sen tallentamista laitteellesi. Ohjelmoijat tekevät tätä voidakseen käyttää sivuston tietoja omassa koodissaan esimerkiksi tiedon hankintaan tai käsittelyyn.

## Miten tehdään:
Käytä Clojurea ja nitkuta websivun tietoja käyttämällä [clj-http](https://github.com/dakrone/clj-http) kirjastoa. Ensiksi, lataa kirjasto projektisi riippuvuuksiin ja tuo se sitten käyttöön. Sitten voit ladata websivun ja tallentaa sen muuttujaan seuraavasti:
```Clojure
(:require [clj-http.client :as client])
(def webpage (client/get "https://example.com"))
```
Tämä lataa websivun ja tallentaa sen varmuudeksi muuttujaan "webpage". Voit sitten käyttää websivun tietoja tarpeesi mukaan.

## Syvemmälle sukellus:
Lataaminen web-sivulta tuli tarpeelliseksi internetin yleistyessä ja tarpeeseen saada tietoa web-sivuilta automaattisesti. Ennen clj-http:n keksimistä, Clojure-ohjelmoijat saivat tehdä tämän lataamalla Java-kirjastoja. On myös muita vaihtoehtoja, kuten käyttää [clj-http-lite](https://github.com/dakrone/clj-http-lite) pienempään ja nopeampaan lataamiseen tai käyttää [clojure.java.io](https://clojure.github.io/clojure/clojure.java.io-api.html) kirjastoa, jos haluat mennä perus Java-tietä.

## Katso myös:
- [Clj-http:n dokumentaatio](https://github.com/dakrone/clj-http)
- [Esimerkki lataamisesta web-sivulta käyttäen Clojurea](https://cachestocaches.com/2017/3/downloading-with-clojure/)
- [Java-luokkakirjasto lataamista varten](https://docs.oracle.com/javase/8/docs/api/java/net/URL.html)