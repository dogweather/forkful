---
title:                "Työskentely yaml:n kanssa"
html_title:           "Clojure: Työskentely yaml:n kanssa"
simple_title:         "Työskentely yaml:n kanssa"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/working-with-yaml.md"
---

{{< edit_this_page >}}

# Miksi

 YAML on yleinen tiedostomuoto, jota käytetään tietojen tallentamiseen ja jakamiseen eri ohjelmistojen ja sovellusten välillä. Clojure-kehittäjänä sinun on tärkeää ymmärtää YAML:aa ja sen toimintaa, jotta voit integroida sen tehokkaasti projekteihisi ja helposti muokata tiedostoja tarpeen mukaan. 

# Miten käyttää YAML:aa Clojurella

YAML-tiedostot ovat luettavissa Clojuren avulla käyttämällä "clj-yaml" kirjastoa. Voit asentaa sen käyttämällä Leiningenin tai Mavenin kautta seuraavalla komennolla:

```
[clj-yaml "0.5.0"]
```

Tämän jälkeen voit ladata kirjaston käyttämällä "require" komentoa:

```
(ns your-namespace
  (:require [clj-yaml.core :as yaml]))
```

YAML-tiedoston lukemiseksi voit käyttää "load" funktiota ja antaa tiedoston polun parametrina:

```
(yaml/load "path/to/file.yaml")
```

Voit myös lukea YAML-tiedoston suoraan merkkijonona käyttämällä "parse" funktiota:

```
(def yaml-string "--- \n name: John\n profession: Developer\n")
(yaml/parse yaml-string)
```

Tulos tulostuu map-rakenteena, jossa YAML:n avaimet ovat Clojure-käsitteitä ja arvot vastaavat niitä merkkijonoina, numeroina tai muita Clojuren tietotyyppeinä.

# Syvemmälle YAML:n maailmaan

YAML:n luomiskyky ei rajoitu vain yksinkertaisiin arvoihin, vaan se tukee myös monimutkaisempia rakenteita, kuten listoja ja sisäkkäisiä mappeja. Voit myös käyttää YAML:n sisältämiä tyyppejä, kuten Boolean ja Null.

YAML on myös erittäin muokattavissa. Voit määrittää oman muotoillun YAML-rakenteen käyttämällä "configure-parser" funktiota ja antamalla haluamasi asetukset parametreina. Voit esimerkiksi määrittää erilaisia sallittuja tietotyyppejä tai käyttää omia custom marshalling funktioita.

# Katso myös

- [Clojure YAML:en ohjelmallinen muotoilu](https://github.com/tmarble/clj-yaml)
- [YAML.org - Tietoa YAML:sta ja sen käytöstä](https://yaml.org/)
- [Clojure - Ohjelmointikieli ja kehitysympäristö](https://clojure.org/)