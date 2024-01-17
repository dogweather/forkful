---
title:                "Väliaikaisen tiedoston luominen"
html_title:           "Clojure: Väliaikaisen tiedoston luominen"
simple_title:         "Väliaikaisen tiedoston luominen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Mikä & Miksi?

Väliaikaisten tiedostojen luominen on tapa tallentaa väliaikaisia tietoja ohjelman suorituksen aikana. Tämä on hyödyllistä esimerkiksi, kun halutaan tallentaa väliaikaisia tuloksia laskutoimitusten välillä.

# Miten:

Väliaikaisten tiedostojen luominen on helppoa Clojuressa seuraavien funktioiden avulla:

```Clojure
(require '[clojure.java.io :as io])

;; Luo väliaikaisen tiedoston valitussa hakemistossa ja palauttaa sen polun:
(io/file "hakemisto" "tiedoston_nimi")

;; Luo väliaikaisen tiedoston oletushakemistoon ja palauttaa sen polun:
(io/file "tiedoston_nimi")
```

Esimerkki:

```Clojure
=> (io/file "tmp" "tiedosto.txt")
/tmp/tiedosto.txt
```

# Syvemmälle:

Väliaikaisten tiedostojen luominen on yleinen tapa ohjelmoinnissa, ja sitä käytetään esimerkiksi väliaikaisten tietojen tallentamiseen tai muokkaamiseen. Clojuren lisäksi väliaikaisten tiedostojen luominen on mahdollista myös muilla ohjelmointikielillä, kuten Pythonilla ja Javalla.

Väliaikaisten tiedostojen luomiseen Clojuressa on myös muita tapoja, kuten käyttämällä ```deftemp``` -makroa tai ```with-open``` -funktiota. Nämä vaihtoehdot tarjoavat lisää joustavuutta ja kontrollointia väliaikaisen tiedoston käytössä.

# Katso myös:

- [Clojuren dokumentaatio](https://clojure.org/api/clojure.java.io)
- [Java'n File-luokka](https://docs.oracle.com/javase/7/docs/api/java/io/File.html)