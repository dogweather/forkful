---
title:    "Clojure: Tarkista onko kansio olemassa"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Monissa Clojure-projekteissa voi käydä niin, että tarvitaan tietty tiedosto tai hakemisto. Ennen kuin aloitetaan koodin suorittaminen, on tärkeää tarkistaa, löytyykö kyseinen hakemisto, jotta vältettäisiin mahdolliset virheet.

## Kuinka tehdä

Tutkitaan ensin, miten tarkistaa, onko hakemisto olemassa. Tämä voidaan tehdä hyödyntämällä "clojure.java.io" -kirjastoa. Ensimmäinen askel on tuoda kirjasto projektiin komennolla ```(require '[clojure.java.io :as io])```. Tämän jälkeen voidaan käyttää io-kirjaston ```file```-funktiota ja antaa sille haluttu polku, jonka olemassaoloa halutaan tarkistaa. Esimerkiksi, jos halutaan tarkistaa, onko hakemisto "tiedostot" olemassa, käytetään funktiota seuraavasti:

```Clojure
(io/file "tiedostot") 
=> #object[java.io.File 0x5dde5f12 "tiedostot"]
```

Kuten nähdään, funktio palauttaa File-olion, eikä vielä suoraan kerro, onko hakemisto olemassa vai ei. Tämän vuoksi tarvitaan toinen funktio, ```exists?```, joka tarkistaa, onko annettu File-olio olemassa. Loppujen lopuksi koodi voisi näyttää esimerkiksi tältä:

```Clojure
(require '[clojure.java.io :as io])

(def hakemisto (io/file "tiedostot"))

(if (io/exists? hakemisto)
  (println "Hakemisto 'tiedostot' on olemassa!")
  (println "Hakemistoa ei löytynyt."))
```

Huomaa, että funktiot ```file``` ja ```exists?``` palauttavat vain boolean-arvoja, jotka ilmaisevat, onko hakemisto olemassa vai ei. Näin ollen niitä voidaan käyttää myös ehdollisissa lauseissa, kuten esimerkissä yllä.

## Syvempi sukellus

Nyt kun olemme oppineet, miten tarkistaa, onko hakemisto olemassa, voimme tutkia hieman syvemmälle aiheeseen. On hyvä huomata, että "clojure.java.io" -kirjaston ```file```-funktio hyväksyy myös vaihtoehtoja (options) parametrina. Jos haluamme esimerkiksi tarkistaa, onko hakemisto olemassa vain, jos se on "pakollinen" (required), voimme antaa parametriksi vector-olion, joka sisältää ```:required```-avaimen:

```Clojure
(def hakemisto (io/file "tiedostot" {:options [:required]}))
(io/exists? hakemisto)
=> false
```

Tällöin funktio palauttaa arvon false, koska hakemistoa ei löydy. Jos taas annamme parametriksi ```{:options [:create]}```, funktio luo hakemiston, jos sitä ei ole olemassa. Voimme myös tarkistaa "pakollinen" ja "luominen" yhdessä näin:

```Clojure
(io/file "tiedostot" {:options [:required :create]})
=> #object[java.io.File 0x5bd838f2 "tiedostot"]
```

Tämä ratkaisu on kätevä, jos halutaan varmistua, että hakemisto on olemassa, mutta samalla luoda se, jos sitä ei löydy. Syvempiin vaihtoehtoihin ja niiden käyttöön kannattaa tutustua [ClojureDocsin](https://clojuredocs.org) kautta.

## Katso myös

- [ClojureDocs](https://clojuredocs.org)
- [Clojure