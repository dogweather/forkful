---
date: 2024-01-20 17:38:08.381675-07:00
description: "How to: (Kuinka tehd\xE4\xE4n:) Clojure k\xE4ytt\xE4\xE4 `clojure.string/lower-case`\
  \ funktiota."
lastmod: '2024-04-05T21:53:57.728298-06:00'
model: gpt-4-1106-preview
summary: "(Kuinka tehd\xE4\xE4n:) Clojure k\xE4ytt\xE4\xE4 `clojure.string/lower-case`\
  \ funktiota."
title: Merkkijonon muuntaminen pieniksi kirjaimiksi
weight: 4
---

## How to: (Kuinka tehdään:)
Clojure käyttää `clojure.string/lower-case` funktiota.

```Clojure
(require '[clojure.string :as str])

(str/lower-case "Moi MAAILMA!") ;=> "moi maailma!"
(str/lower-case "Hyvää Päivää!") ;=> "hyvää päivää!"
(str/lower-case "123 ABC def!")  ;=> "123 abc def!"
```

Saat saman tuloksen jokaiselle esimerkille.

## Deep Dive (Sukellus Syvyyksiin)
Alkulähteissä, merkkijonot oli vain isoja kirjaimia. Pienet kirjaimet luotiin selkeyden vuoksi. Clojuren `lower-case` toimii Unicode-tietojen kanssa, mikä tarkoittaa että se toimii eri kielillä, ei pelkästään englannissa.

Vaihtoehtona, voit itse käydä merkkijonon läpi ja muuttaa kirjaimet käyttäen Java metodeja, koska Clojure on JVM-pohjainen.

```Clojure
(.toLowerCase "Tämä on Testi!") ;=> "tämä on testi!"
```

Mutta Clojuren standardikirjasto on usein puhdas ja lyhyempi tapa toteuttaa tämä.

Koodi toimii kutsuen Java String `toLowerCase` metodia sisäisesti, mikä takaa nopeuden ja varmuuden.

## See Also (Katso Myös)
- Clojure Docs: [clojure.string/lower-case](https://clojuredocs.org/clojure.string/lower-case)
- Oracle Java Docs: [`String.toLowerCase`](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
