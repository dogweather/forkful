---
title:                "Komennoriviparametrien lukeminen"
aliases:
- fi/clojure/reading-command-line-arguments.md
date:                  2024-01-20T17:55:35.535415-07:00
model:                 gpt-4-1106-preview
simple_title:         "Komennoriviparametrien lukeminen"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Komennoriviparametrien lukeminen tarkoittaa argumenttien vastaanottamista suoraan ohjelmalle, kun sen käynnistää terminaalista. Ohjelmoijat tekevät sen, jotta voivat antaa dynaamisia syötteitä ja muuttaa ohjelman toimintaa lennosta.

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
