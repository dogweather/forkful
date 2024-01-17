---
title:                "Aloittaminen uusi hanke"
html_title:           "Clojure: Aloittaminen uusi hanke"
simple_title:         "Aloittaminen uusi hanke"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?

Aloittaminen uuden projektin kanssa tarkoittaa uuden ohjelmiston, työkalun tai sovelluksen luomista. Ohjelmoijat tekevät tätä parantaakseen tai laajentaakseen olemassa olevia projektejaan tai luodakseen jotain aivan uutta.

## Miten:

Käytä Clojurea aloittaaksesi uusi projektisi helposti ja tehokkaasti:

```Clojure
(ns uusi-projekti.core
  (:gen-class))

(defn -main
  "Tämä funktio suoritetaan kun ohjelma käynnistetään"
  [& args]
  (println "Tervetuloa uuden projektin maailmaan!"))
```

Tuloste: Tervetuloa uuden projektin maailmaan!

Tässä esimerkissä määritämme uuden projektin Clojure-koodin avulla ja tulostamme yksinkertaisen viestin käynnistyksen yhteydessä.

## Syväsukellus:

Historiallisesti Clojure on kehitetty parantamaan Javaa ja toimimaan sen päällä, mutta sen suosio on kasvanut myös itsenäisenä ohjelmointikieleenä. Clojure tarjoaa puhtaamman ja yksinkertaisemman syntaksin kuin Java, mikä tekee siitä hyvän vaihtoehdon monet monimutkaisille projekteille.

Clojurella on myös laaja ja aktiivinen yhteisö, joka tarjoaa tukea ja apua projekteissa. Lisäksi Clojuren avoin lähdekoodi tekee siitä helpon integroida muiden teknologioiden kanssa.

## Katso myös:

Halutessasi tutustua aiheeseen tarkemmin, suosittelemme seuraavia lähteitä:

- [Clojuren virallinen verkkosivusto] (https://clojure.org/)
- [Clojuren käyttöönotto] (https://clojure.org/guides/getting_started)
- [Clojuren GitHub-sivu] (https://github.com/clojure/clojure)