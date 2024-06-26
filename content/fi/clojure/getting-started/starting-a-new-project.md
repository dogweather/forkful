---
date: 2024-01-20 18:03:21.068200-07:00
description: "How to - Kuinka Tehd\xE4: Clojure-projektien aloittaminen juontaa juurensa\
  \ Lisp-ohjelmointikieleen, joka on yksi vanhimmista ohjelmointikielist\xE4. Leiningen\u2026"
lastmod: '2024-04-05T22:51:10.343166-06:00'
model: gpt-4-1106-preview
summary: "Clojure-projektien aloittaminen juontaa juurensa Lisp-ohjelmointikieleen,\
  \ joka on yksi vanhimmista ohjelmointikielist\xE4."
title: Uuden projektin aloittaminen
weight: 1
---

## How to - Kuinka Tehdä:
```Clojure
; Asenna Leiningen, Clojure-projektien hallintatyökalu
; Luo uusi projektikomento:
lein new app minun-clojure-projektini

; Projektirakenteen tulisi näyttää tältä:
; minun-clojure-projektini/
; ├── project.clj
; ├── src/
; │   └── minun_clojure_projektini/
; │       └── core.clj
; ├── test/
; │   └── minun_clojure_projektini/
; │       └── core_test.clj
; └── resources/

; Aloita koodaaminen src/minun_clojure_projektini/core.clj:ssä.
(ns minun-clojure-projektini.core)
(defn tervehdi [] (println "Tervehdys, Clojure maailma!"))

; Käynnistä REPL ja kutsu tervehdi-funktio
; REPL:ssä:
(tervehdi)
; Tulostuu:
Tervehdys, Clojure maailma!
```

## Deep Dive - Syväsukellus
Clojure-projektien aloittaminen juontaa juurensa Lisp-ohjelmointikieleen, joka on yksi vanhimmista ohjelmointikielistä. Leiningen vaihtoehtona, voit käyttää myös tools.deps ja `clj`-komentoa projektien hallintaan. Leiningenen `project.clj` tiedosto määrittää riippuvuudet ja muut konfiguraatiot. `src` hakemisto sisältäisi lähteen koodit ja `test` sisältää yksikkötestit.

## See Also - Katso Myös
- [Clojure.org virallinen sivusto](https://clojure.org)
- [Leiningen kotisivu](https://leiningen.org)
- [ClojureDocs - Alue ohjelmistokehittäjien oppimiseen ja jakamiseen](https://clojuredocs.org)
- [Clojure Style Guide](https://guide.clojure.style)
