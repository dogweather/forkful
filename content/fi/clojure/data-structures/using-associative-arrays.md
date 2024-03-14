---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:10:27.080917-07:00
description: "Assosiatiiviset taulukot eli hajautustaulut Clojuressa mahdollistavat\
  \ tietojen tallentamisen ja hakemisen avain-arvo -pareina. Ne ovat avainratkaisu\u2026"
lastmod: '2024-03-13T22:44:56.177566-06:00'
model: gpt-4-0125-preview
summary: "Assosiatiiviset taulukot eli hajautustaulut Clojuressa mahdollistavat tietojen\
  \ tallentamisen ja hakemisen avain-arvo -pareina. Ne ovat avainratkaisu\u2026"
title: "Assosiatiivisten taulukoiden k\xE4ytt\xF6"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Assosiatiiviset taulukot eli hajautustaulut Clojuressa mahdollistavat tietojen tallentamisen ja hakemisen avain-arvo -pareina. Ne ovat avainratkaisu rakenteellisen datan hallintaan, tehden tiettyjen elementtien hakemisesta nopeampaa ilman listan läpikäyntiä.

## Kuinka:

Clojuressa assosiatiivisten taulukoiden (hajautustaulujen) luominen ja käsittely on suoraviivaista. Sukelletaan sisään esimerkkien kautta.

Hajautustaulun luominen:

```clojure
(def my-map {:name "Alex" :age 30})
```

Voit hakea arvon määrittämällä sen avaimen:

```clojure
(get my-map :name)
;; "Alex"
```
Tai idiomimmalla tavalla, voit käyttää avainta funktiona:

```clojure
(:name my-map)
;; "Alex"
```

Merkintöjen lisääminen tai päivittäminen on yksinkertaista:

```clojure
(def updated-map (assoc my-map :location "New York"))
;; {:name "Alex", :age 30, :location "New York"}

(def incremented-age (update my-map :age inc))
;; {:name "Alex", :age 31}
```

Avaimien poistamiseen käytä `dissoc`:

```clojure
(def removed-age (dissoc my-map :age))
;; {:name "Alex"}
```

Käydessä läpi karttaa:

```clojure
(doseq [[k v] my-map] (println k "->" v))
;; :name -> Alex
;; :age -> 30
```

Ja ehdolliseen pääsyyn, `find` palauttaa avain-arvo -parin, jos avain löytyy:

```clojure
(find my-map :age)
;; [:age 30]
```

## Syväsukellus

Clojuren assosiatiiviset taulukot, joita yleisesti kutsutaan myös hajautustauluiksi, ovat uskomattoman monipuolisia ja tehokkaita avain-arvo -pohjaisen datan hallintaan. Ne ovat osa Clojuren rikasta kokoelmakirjastoa, syvällä kielen filosofiassa, joka korostaa muuttumattomuutta ja funktionaalista ohjelmointia. Toisin kuin taulukot tai listat, jotka vaativat O(n) aikavaativuuden elementtien hakemiseen, hajautustaulut tarjoavat lähes vakioajan aikavaativuuden haettessa, tehden niistä erittäin tehokkaita hakutoiminnoissa.

Voisi väittää, että Clojuren vektorit voisivat palvella samankaltaista tarkoitusta indeksoinnin kautta, mutta hajautustaulut loistavat käsiteltäessä epäjärjestäytynyttä ja merkityksellä varustettua dataa, jossa avain tarjoaa merkityksellisen kuvauksen pikemminkin kuin mielivaltainen indeksi.

Ominainen Clojurelle (ja sen Lisp-perinnölle), assosiatiiviset taulukot ovat ensiluokkaisia kansalaisia, tarkoittaen, että niitä voidaan suoraan manipuloida, välittää funktioihin ja enemmän, ilman erikoissyntaksia tai pääsytapoja. Tämä suunnittelupäätös vahvistaa Clojuren painotusta yksinkertaisuuteen ja voimaan.

Vaikka hajautustaulut ovat uskomattoman hyödyllisiä, on mainitsemisen arvoista, että erittäin suurille datamäärille tai skenaarioille, joissa avaimet ovat erittäin dynaamisia (jatkuva lisäys ja poisto), vaihtoehtoiset tietorakenteet tai tietokannat saattavat tarjota parempaa suorituskykyä ja joustavuutta. Kuitenkin, useimmissa tyypillisissä käyttötapauksissa Clojure-sovellusten alueella, assosiatiiviset taulukot tarjoavat kestävän ja tehokkaan keino datan hallintaan.
