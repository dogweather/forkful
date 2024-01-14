---
title:                "Clojure: Työskentely jsonin kanssa"
simple_title:         "Työskentely jsonin kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-json.md"
---

{{< edit_this_page >}}

## Miksi

Miksi työskennellä JSON:n kanssa? JSON (JavaScript Object Notation) on yleisesti käytetty tietojen tallennusmuoto, joka on kätevä ja helppo lukea ja kirjoittaa. Se on myös lightweight ja sopii hyvin internet-sovellusten käyttöön, mikä tekee siitä erittäin suositun ohjelmointikielen Clojure yhteisössä.

## Miten

Toimiminen JSON:n kanssa Clojurella on hyvin yksinkertaista. Voit käyttää Clojure.data.json -kirjastoa muuntaaksesi Clojure mallit JSON muotoon ja päinvastoin. Seuraavassa on esimerkki:

```Clojure
(require '[clojure.data.json :as json])

;; Convert Clojure map into JSON
(json/write-str {:name "John" :age 30})

;; Output: "{\"name\":\"John\",\"age\":30}"

;; Convert JSON into Clojure map
(json/read-str "{\"name\":\"John\",\"age\":30}")

;; Output: {:name "John" :age 30}
```

JSON:n lukeminen ja kirjoittaminen Clojuren avulla on vaivatonta ja intuitiivista. Kirjastossa on myös muita hyödyllisiä funktioita, kuten `json/read` ja `json/write`, jotka käsittelevät tiedostoja tai IO-virtoja JSON-muodossa.

## Syventyvä tutkimus

JSON:n käyttö Clojurella voi olla monipuolisempaa kuin vain yksinkertainen muuntaminen malleja ja tekstimuotoisen datan välillä. Voit myös käyttää JSON tiedostoja suoraan Clojure koodissa Clojure data rakenteina. Tämä voidaan tehdä käyttämällä `json/read` funktiota luomaan Clojure mappeja tai vektoreita JSON-tiedostosta.

Toinen kätevä tapa käsitellä JSON-dataa Clojurella on käyttää `cheshire` kirjastoa, joka tarjoaa lisäominaisuuksia, kuten automaattisen koodauksen ja dekoodauksen, sekä monimutkaisemman JSON-syntaksin käsittelyn.

## Katso myös

- Virallinen Clojure.data.json dokumentaatio: https://clojure.github.io/clojure/data.json-api.html
- Cheshire dokumentaatio: https://github.com/dakrone/cheshire/wiki

Tee elämästäsi helpompaa JSON:n kanssa käyttämällä Clojurea ja näitä käteviä kirjastoja!