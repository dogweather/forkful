---
title:    "Clojure: Uuden projektin aloittaminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä, miksi aloittaa uusi ohjelmointiprojekti Clojure-kielillä. Clojure on dynaaminen ohjelmointikieli, joka yhdistää funktionaalisen ohjelmoinnin ja vahvan Java-yhteensopivuuden.

## Kuinka aloittaa

Clojuren asentaminen ja aloittaminen on helppoa. Tarvitset vain Clojuren asennetun JDK:n (Java Development Kitin) ja tekstieditorin. Seuraavassa esimerkissä näytämme, miten tulostaa "Hei maailma!" käyttäen Clojurea.

```Clojure
(ns project.core
  (:gen-class))

(defn -main []
  (println "Hei maailma!"))
```

Tallenna tämä tiedosto nimellä `core.clj` ja suorita se terminaalissa komennolla `clojure core.clj`. Näet tulosteen "Hei maailma!" terminaalissa.

## Syvällinen sukellus

Ennen kuin aloitat uuden Clojure-projektin, on hyvä tutustua kielen syntaksiin ja perusominaisuuksiin. Clojuren virallinen sivusto tarjoaa kattavia oppaita ja dokumentaatiota, jotka auttavat sinua pääsemään alkuun. Voit myös etsiä verkosta aiheisiin liittyviä blogiartikkeleita ja ilmaisia ​​verkkokursseja.

## Katso myös

- [Clojuren virallinen sivusto](https://clojure.org)
- [Clojuren opiskelu -ohjelma](https://www.clojure.org/learn)
- [Clojuren dokumentaatio](https://clojure.org/api/api)
- [Clojuren oppimateriaalit ja kurssit](https://www.clojure.org/learn/resources)
- [Clojure-yhteisö Suomessa](http://www.clojure.fi/)