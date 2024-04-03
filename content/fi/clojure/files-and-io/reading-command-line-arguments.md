---
date: 2024-01-20 17:55:35.535415-07:00
description: "How to: Clojure-ohjelmissa komennoriviparametrien k\xE4ytt\xF6 on suoraviivaista.\
  \ T\xE4ss\xE4 on helppo esimerkki."
lastmod: '2024-03-13T22:44:56.199640-06:00'
model: gpt-4-1106-preview
summary: "Clojure-ohjelmissa komennoriviparametrien k\xE4ytt\xF6 on suoraviivaista."
title: Komennoriviparametrien lukeminen
weight: 23
---

## How to:
Clojure-ohjelmissa komennoriviparametrien käyttö on suoraviivaista. Tässä on helppo esimerkki:

```Clojure
;; Sovelluksen pääfunktio, joka tulostaa komennoriviparametrit
(defn -main [& args]
  (println "Antamasi argumentit ovat:")
  (doseq [arg args]
    (println arg)))

;; Sovelluksen suoritus terminaalissa
;; komento: lein run Moi Maailma

;; Tulostuu:
;; Antamasi argumentit ovat:
;; Moi
;; Maailma
```

## Deep Dive
Command line arguments -historia ulottuu aikaisiin Unix-päiviin. Clojure, mikä pyörii JVM:llä, perii Javan tapoja käsitellä argumentteja. `*command-line-args*` -globaali muuttuja oli aikaisemmin käytössä, mutta `-main` funktion `& args` on nykyinen tapa.

Vaihtoehtoina komennorivisyötteille löytyy erilaisia kirjastoja, esimerkiksi `tools.cli`, joka tarjoaa kehittyneempiä analyysityökaluja ja validaatiota.

Implementointi yksityiskohdat riippuvat siitä, mitä ohjelmalla halutaan tehdä argumenteilla. Yksinkertaisesti ne voidaan tulostaa, kuten yllä, tai käyttää ohjauksessa ja konfiguraatiossa.

## See Also
- [ClojureDocs - Command Line Arguments Example](https://clojuredocs.org/clojure.core/*command-line-args*)
- [GitHub - tools.cli](https://github.com/clojure/tools.cli)
