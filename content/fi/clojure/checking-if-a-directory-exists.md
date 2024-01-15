---
title:                "Tarkistetaan, onko hakemisto olemassa."
html_title:           "Clojure: Tarkistetaan, onko hakemisto olemassa."
simple_title:         "Tarkistetaan, onko hakemisto olemassa."
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Miksi

On usein tarpeellista tarkistaa, onko tietty hakemisto olemassa Clojure-ohjelmassa. Tämä mahdollistaa esimerkiksi turvallisen tiedostojen käsittelyn ja välttää ohjelman kaatumisen, jos hakemistoa ei löydy.

## Miten

Tässä on esimerkki siitä, miten tarkistaa, onko hakemisto olemassa Clojurella:

```Clojure
(ns directory-check
  (:require [clojure.java.io :as io]))

(defn exists? [dir]
  (.exists (io/file dir)))

(println (exists? "/Users/example/directory")) ; tulostaa true
(println (exists? "/Users/example/nonexistent")) ; tulostaa false
```

## Syventävä tieto

Clojurella hakemistojen tarkistaminen tapahtuu `.exists`-funktion avulla, joka tarkistaa, onko tiedostopolku olemassa. Tämä funktio palauttaa joko true tai false riippuen siitä, onko hakemisto olemassa vai ei.

Toinen tapa tarkistaa hakemiston olemassaolo on käyttää `clojure.java.io/file-exists?`-funktiota, joka palauttaa true, jos hakemisto on olemassa. Se kuitenkin heittää poikkeuksen, jos annettua hakemistoa ei ole.

## Katso myös

- Clojuren virallinen dokumentaatio `.exists`-funktiosta: https://clojure.github.io/clojure/clojure.java.io-api.html#clojure.java.io/file_exists? 
- Lisätietoja tiedostojen ja hakemistojen käsittelystä Clojuressa: https://www.braveclojure.com/files-and-directories/