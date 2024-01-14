---
title:                "Clojure: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

## Miksi: Miksi YAML:n käyttäminen kannattaa?

YAML (Yet Another Markup Language) on ihmisläheinen ja helppolukuinen tapa tallentaa ja jakaa tietoa tietokoneiden välillä. Se on myös suosittu valinta konfiguraatiotiedostojen muodossa. YAML on melko yksinkertainen, mutta silti voi olla hieman haastavaa perehtyä sen käyttöön. Tässä blogikirjoituksessa annamme sinulle ohjeet YAML:n käyttämiseen Clojuren ohjelmointikielellä.

## Kuinka: Esimerkkejä ja koodilohkoja

YAML:n käyttö Clojuressa vaatii muutaman alustavan askeleen. Ensinnäkin, meidän täytyy lisätä YAML riippuvuus projektiimme, mikä voidaan tehdä seuraavalla koodirivillä:

```Clojure
[org.yaml/yaml "0.1.7"]
```

Kun tämä on tehty, voimme aloittaa YAML-tiedostojen lukemisen ja kirjoittamisen. Seuraavassa esimerkissä luemme YAML-tiedostosta käyttäjän nimen ja iän ja tulostamme ne konsolille:

```Clojure
(ns yaml-example.core
  (:require [yaml.core :as yaml]))

(def user (yaml/load-file "kayttaja.yaml"))
(println "Käyttäjän nimi: " (:name user))
(println "Käyttäjän ikä: " (:age user))
```

Jos YAML-tiedosto "kayttaja.yaml" näyttää tältä:

```yaml
name: Johanna
age: 28
```

Saat seuraavanlaisen tulosteen:

```
Käyttäjän nimi: Johanna
Käyttäjän ikä: 28
```

Voit myös luoda uusia YAML-tiedostoja yksinkertaisella koodilla:

```Clojure
(ns yaml-example.core
  (:require [yaml.core :as yaml]))

(def user {:name "Mikko"
           :age 32})

(yaml/spit "uusi.yaml" user)
```

Tämä luo tiedoston nimeltä "uusi.yaml" seuraavalla sisällöllä:

```yaml
name: Mikko
age: 32
```

## Syvemmälle: Työskentelyä YAML:n kanssa

YAML:n syvemmälle perehtyminen vaatii hieman enemmän aikaa ja vaivaa, mutta se voi olla hyödyllistä, jos käsittelet monimutkaisempia tietorakenteita. Voit lukea lisätietoja YAML:n käyttömahdollisuuksista Clojuressa esimerkiksi [YAML-kirjaston dokumentaatiosta](https://github.com/borkdude/yaml-clj) tai seuraavasta [artikkelista](http://yobriefca.se/blog/2014/06/17/yaml-files-and-clojure.html).

## Katso myös

- [YAML syntaksin opas](https://yaml.org/spec/1.2/spec.html)
- [JSON vs YAML vertailu](https://yaml.org/spec/1.2/spec.html)
- [Lisää tietoa Clojusta](https://clojure.org/)