---
title:                "Työskentely jsonin kanssa"
html_title:           "Clojure: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi
Haluatko käsitellä dataa, joka on helppo lukea ja muokata? JSON on yleinen tiedostomuoto, joka sopii hyvin tähän tarkoitukseen ja Clojure tekee sen käytöstä vieläkin helpompaa!

## Kuinka tehdä
```clojure
;; Luodaan uusi JSON-muotoinen data rakenteessa
(def data {:nimi "Juha" :ika 32 :harrastukset ["luistelu" "kirjoittaminen" "retkeily"]})

;; Muuntaa Clojure-rakenteen JSON-muotoon
(prn (json/write-str data))
;; {"nimi":"Juha","ika":32,"harrastukset":["luistelu","kirjoittaminen","retkeily"]}

;; Lukee JSON-tiedoston ja tallentaa sen Clojure-muotoon
(def muistiinpanot (json/read-str (slurp "muistiinpanot.json")))
;; {:otsikot ["Ensimmäinen päivä" "Toinen päivä" "Kolmas päivä"] :merkintä 1 "Tunteet" "Onnellinen ja energinen"}

;; Muokkaa JSON-dataa
(def uusi-muistiinpano (assoc muistiinpanot :neljäs-päivä "Hopeakannus"))
(prn (json/write-str uusi-muistiinpano))
;; {"otsikot":["Ensimmäinen päivä","Toinen päivä","Kolmas päivä","Neljäs päivä"],"merkintä 1":"Tunteet","tyyppi":"Onnellinen ja energinen"}

;; Poistaa tietoa JSON-datasta
(def poistettu-muistiinpanot (dissoc muistiinpanot :merkintä 1))
(prn (json/write-str poistettu-muistiinpanot))
;; {"otsikot":["Ensimmäinen päivä","Toinen päivä","Kolmas päivä"],"tyyppi":"Onnellinen ja energinen"}
```

## Syventävä tarkastelu
JSON-tiedostoja käytettäessä on tärkeää muistaa, että muuttujien nimet tulee olla merkkijonoina ja datarakenteet tulee olla johdettu Clojure-rakenteista. JSON-tiedostot ovat myös käteviä, kun haluat kommunikoida eri ohjelmointikielten välillä, sillä ne ovat melko universaaleja ja helppoja lukea.

## Katso myös
- [Clojure virallinen kotisivu](https://clojure.org/)
- [JSON muotoilun opas](https://www.json.org/) 
- [Clojure asetustiedoston tallentaminen JSON-muotoon](https://github.com/lgrapenthin/clojure-bind/blob/master/doc/upstart.md)