---
date: 2024-01-26 03:43:33.205790-07:00
description: "Kuinka: Clojuressa k\xE4yt\xE4mme p\xE4\xE4asiassa `Math/round`, `Math/floor`\
  \ ja `Math/ceil`."
lastmod: '2024-03-13T22:44:56.179551-06:00'
model: gpt-4-0125-preview
summary: "Clojuressa k\xE4yt\xE4mme p\xE4\xE4asiassa `Math/round`, `Math/floor` ja\
  \ `Math/ceil`."
title: "Numerojen py\xF6rist\xE4minen"
weight: 13
---

## Kuinka:
Clojuressa käytämme pääasiassa `Math/round`, `Math/floor` ja `Math/ceil`:

```clojure
(Math/round 3.5) ; => 4
(Math/round 3.4) ; => 3

(Math/floor 3.7) ; => 3.0
(Math/ceil 3.2)  ; => 4.0
```

Tietyille desimaalipaikoille me kerromme, pyöristämme ja jaamme:

```clojure
(let [num 3.14159
      scale 1000]
  (/ (Math/round (* num scale)) scale)) ; => 3.142
```

## Syväsukellus
Ennen hienoja ohjelmointikieliä, pyöristäminen oli manuaalinen prosessi, ajattele helmilaskinta tai paperia. Ohjelmoinnissa se on olennaista numeron esityksen kannalta liukulukujen tarkkuuden rajoitusten vuoksi.

Vaihtoehtoja pyöristämiselle ovat `BigDecimal`-luokan käyttö tarkkuuden hallintaan tai kirjastot kuten `clojure.math.numeric-tower` edistyneitä matemaattisia toimintoja varten. Clojuren `Math/round` perustuu Javan `Math.round`, `Math/floor` ja `Math/ceil` -funktioihin, mikä tarkoittaa, että se perii samat liuku- ja kaksoistarkkuuden nyanssit.

Toteutuksen osalta, kun pyöristät Clojuressa, muista, että se käyttää automaattisesti kaksoistarkkuutta käsitellessään desimaaleja. Varo pyöristysvirheitä!

## Katso Myös
- Clojure Math API: [https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*](https://clojure.github.io/clojure/clojure.core-api.html#clojure.core/*math-context*)
- Java Math API: [https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html](https://docs.oracle.com/javase/8/docs/api/java/lang/Math.html)
- Ymmärtäminen liukulukujen tarkkuudesta: [https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html](https://docs.oracle.com/cd/E19957-01/806-3568/ncg_goldberg.html)
