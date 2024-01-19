---
title:                "Aloittaminen uuden projektin"
html_title:           "C: Aloittaminen uuden projektin"
simple_title:         "Aloittaminen uuden projektin"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä ja Miksi?
Uuden projektin aloittaminen tarkoittaa uuden ohjelman, sovelluksen tai palvelun luomista alusta alkaen. Ohjelmoijat tekevät niin uusien ideoiden ja innovaatioiden toteuttamiseksi ja ongelmien ratkaisemiseksi.

## Miten:
Clojure, joka on toiminnallinen ohjelmointikieli, mahdollistaa uuden projektin luomisen sujuvasti. Aloitetaan koodiesimerkeillä. 

```Clojure
;; Projektipohjan luominen Leiningen-kirjaston avulla
lein new awesome-project
```
Tämä luo uuden projektin nimellä "awesome-project". Aivan yksinkertaista!

## Syvällä Sukellus:
Clojure, joskin nuori kieli, on saavuttanut suosiota sen toiminnallisen ohjelmoinnin mallin ja suoraviivaisen lähestymistavan ansiosta. Historiallisesti Clojure on otettu erittäin hyvin vastaan Java-yhteisössä, koska sen JVM-suoritusympäristö. Vaikka Clojure-ohjelmien aloittaminen on helppoa Leiningenin avulla, vaihtoehtoja on myös. Boot ja Clojure CLI/Deps.edn ovat muita arvostettuja työkaluja.

Clojure-projektin toteutuksessa on aina hyvä pitää mielessä, että sen ytimessä sen tulisi olla toiminnallinen, muuttumaton ja olemaan huolissaan sivuvaikutuksista.

## Katso myös:
Seuraavassa on joitakin hyödyllisiä linkkejä, jotka voivat auttaa sinua ymmärtämään Clojurea ja uuden projektin luomista paremmin:

1. Clojuren virallinen opas: https://clojure.org/guides/getting_started
2. Leiningen-kirjastosta: https://leiningen.org/
3. Clojure-projektien luominen Clojure CLI:n avulla: https://clojure.org/guides/deps_and_cli
4. Boot-kirjasto: http://boot-clj.com/