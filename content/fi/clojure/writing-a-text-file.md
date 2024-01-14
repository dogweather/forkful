---
title:                "Clojure: Tekstitiedoston kirjoittaminen"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Miksi

Tekstin kirjoittaminen on tärkeä osa ohjelmointia, sillä se mahdollistaa datan tallentamisen ja käsittelyn helpommin. Lisäksi tekstitiedoston avulla voi luoda käyttäjäystävällisiä ohjelmia.

## Miten

Tekstin kirjoittaminen Clojurella on yksinkertaista. Tässä on esimerkki, kuinka voit luoda uuden tekstitiedoston ja kirjoittaa siihen tekstin:

```Clojure
(with-open [file (clojure.java.io/writer "testi.txt")]
    (.write file "Tervetuloa ohjelmoimaan Clojurella!"))
```

Tämän jälkeen tekstitiedosto "testi.txt" sisältää tekstin "Tervetuloa ohjelmoimaan Clojurella!".

## Syvällinen sukellus

Clojure tarjoaa mahdollisuuden kirjoittaa monimutkaisempiakin tekstitiedostoja. Voit esimerkiksi käyttää Clojuren lukijoita ja kirjoittajia (readers and writers) luodaksesi CSV-tiedostoja tai jopa JSON-tiedostoja.

Esimerkiksi, luodaan JSON-tiedosto, joka sisältää listan henkilöistä heidän nimiensä ja ikänsä kanssa:

```Clojure
(require '[clojure.data.json :as json])

(def henkilot [{:nimi "Matti" :ika 30}
               {:nimi "Maija" :ika 28}])

(with-open [writer (clojure.java.io/writer "henkilot.json")]
  (.write writer (str (json/write-str henkilot))))
```

Tämän jälkeen voit nähdä "henkilot.json" -tiedostossa JSON-muotoisen listan:

```JSON
[{"nimi": "Matti", "ika": 30},
{"nimi": "Maija", "ika": 28}]
```

Tämä on vain yksi esimerkki siitä, kuinka tekstiä voidaan käyttää monipuolisesti Clojurella.

## Katso myös

1. [Clojuren tekstien käsittely](https://clojuredocs.org/clojure.string)
2. [Lisätietoa Clojuressa tekstin käsittelystä](https://www.braveclojure.com/sequences/)
3. [JSON-tiedostojen luominen Clojurella](https://clojuredocs.org/clojure.data.json)