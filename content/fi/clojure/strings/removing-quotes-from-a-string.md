---
title:                "Merkkijonosta lainausmerkkien poistaminen"
date:                  2024-01-26T03:39:07.668911-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lainausmerkkien poistaminen merkkijonosta tarkoittaa kiusallisten kaksin- tai yksinkertaisten lainausmerkkien hävittämistä tekstin ympäriltä. Ohjelmoijat tekevät tämän puhdistaakseen dataa, varmistaakseen yhtenäisyyden tai valmistaakseen merkkijonot käsittelyyn, jossa lainausmerkit ovat ei-toivottuja tai voivat aiheuttaa virheitä.

## Kuinka:
Clojuressa merkkijonot ovat muuttumattomia, joten kun puhumme "lainausmerkkien poistamisesta", puhumme oikeastaan uuden merkkijonon luomisesta ilman lainausmerkkejä. Tässä tiivistelmä käyttäen `clojure.string/replace`:

```clojure
(require '[clojure.string :as str])

; Heitetäänpä nuo kaksoislainausmerkit mäkeen
(defn remove-double-quotes [s]
  (str/replace s #"\"" ""))

; Ja potkaistaan ulos yksinkertaiset lainausmerkit
(defn remove-single-quotes [s]
  (str/replace s #"\'" ""))

; Esimerkkikäyttö:
(remove-double-quotes "\"Hei, maailma!\"") ; => "Hei, maailma!"
(remove-single-quotes "'Hei, maailma!'")   ; => "Hei, maailma!"
```
Haluatko käsitellä sekä yksinkertaiset että kaksoislainausmerkit yhdellä iskulla? Kurkista tähän:

```clojure
(defn remove-quotes [s]
  (str/replace s #"[\"\']" ""))

; Esimerkkikäyttö:
(remove-quotes "\"Hei, 'Clojure' maailma!\"") ; => "Hei, Clojure maailma!"
```

## Syväsukellus
Aikoinaan, kun data oli sotkuisempaa kuin lapsen makuuhuone, lainausmerkit merkkijonoissa olivat normi tekstin merkitsemiseksi. Mutta tietojenkäsittelytieteen kehittyessä lainausmerkit muuttuivat enemmäksi kuin vain tekstierottimiksi - niille annettiin myös syntaktisia rooleja ohjelmointikielissä.

Clojure, sen Lisp-perinnön ansiosta, ei käytä lainausmerkkejä samalla tavalla kuin jotkin muut kielet saattavat. Ne ovat toki merkkijonojen merkitsemiseen, mutta niillä on myös erityinen rooli literaalien luomisessa. Riippumatta siitä, lainausmerkkien poistaminen merkkijonoista pysyy ajattomana tehtävänä.

Miksi et vain leikkaa merkkijonon päitä? No, se olettaa lainausmerkkiesi aina halaavan merkkijonosi alkua ja loppua kuin pari ylisöpöilevää isovanhempaa. Todellinen data on sotkuisempaa. Tässä tulevat avuksi regex (säännölliset lausekkeet), joiden avulla voit tähdätä noihin lainausmerkkeihin riippumatta siitä, missä ne piileskelevät.

Vaihtoehtoja? Toki, voit hifistellä `subs`, `trim`, `triml`, `trimr`:llä tai jopa transduserilla, jos haluat rehvastella. Mutta `replace` regex:llä on kuin toisit valomiekan veitsitaisteluun - se menee suoraan asiaan.

## Katso Myös
Jos aivosi kaipaavat lisää Clojuren merkkijonojen käsittelyn herkkuja, nämä vihjeet saattavat auttaa:

- ClojureDocs `clojure.string/replace`-osoitteesta: https://clojuredocs.org/clojure.string/replace
- Säännölliset lausekkeet Clojuressa: https://clojure.org/guides/learn/syntax#_regex
- Java-interoptio merkkijonojen käsittelyyn (Clojurehan pyörii JVM:llä): https://clojure.org/reference/java_interop#_working_with_strings

Älä pysähdy vain lainausmerkkien poistamiseen. Clojure-maassa on kokonainen maailma merkkijonotaikuutta odottamassa löytämistään.
