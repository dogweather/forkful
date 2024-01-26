---
title:                "YAML-tiedostojen käsittely"
html_title:           "Arduino: YAML-tiedostojen käsittely"
simple_title:         "YAML-tiedostojen käsittely"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why?
YAML on datan serialisointikieli, jota käytetään säätöjen, asetusten ja ohjelmoitavien sovellusten tietojen tallennukseen. Ohjelmoijat käyttävät YAMLia, koska se on ihmisen luettavissa ja sillä voidaan kuvata monimutkaisia rakenteita selkeästi.

## How to:
Clojuren YAML-käsittely vaatii kirjaston, esimerkiksi `clj-yaml`. Asenna se lisäämällä projektisi `deps.edn`-tiedostoon:

```Clojure
{:deps {clj-yaml {:mvn/version "0.7.0"}}}
```

Lue YAML-tiedosto ja muunna se Clojure-mapiksi:

```Clojure
(require '[clj-yaml.core :as yaml])

(defn lue-yaml-tiedosto [polku]
  (with-open [rdr (java.io.FileReader. polku)]
    (yaml/parse-string (slurp rdr))))

(def yaml-data (lue-yaml-tiedosto "config.yaml"))
```

Tallenna Clojure-map YAML-tiedostoksi:

```Clojure
(require '[clj-yaml.core :as yaml])

(defn tallenna-yaml-tiedosto [data polku]
  (spit polku (yaml/generate-string data)))
 
(tallenna-yaml-tiedosto {:asiakas {:id 123, :nimi "Yritys Oy"}} "uusi-config.yaml")
```

Näyte `uusi-config.yaml`-tiedostosta:

```yaml
asiakas:
  id: 123
  nimi: "Yritys Oy"
```

## Deep Dive:
YAML (YAML Ain't Markup Language) julkaistiin alun perin 2001. Se suunniteltiin yksinkertaistamaan XML:n käyttöä ja nykyään se on usein valinta rakenne- ja konfiguraatiotiedostoihin. Vaihtoehtoja YAMLille ovat JSON ja TOML. Clojure-kielessä YAMLia käsitellään muuntamalla YAML-stringit Clojure-dattostruktuureiksi ja takaisin, yleensä kirjaston avulla, kuten clj-yaml.

## See Also:
- clj-yaml GitHub-sivu: https://github.com/clj-commons/clj-yaml
- Clojure virallinen sivusto: https://clojure.org
- YAML virallinen sivusto: https://yaml.org
- JSON: https://www.json.org/json-fi.html
- TOML: https://github.com/toml-lang/toml
