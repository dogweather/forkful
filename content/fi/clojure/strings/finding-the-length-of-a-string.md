---
date: 2024-01-20 17:47:03.838360-07:00
description: "How to: Clojure-kieless\xE4 merkkijonon pituuden saa selville `count`-funktiolla."
lastmod: '2024-03-13T22:44:56.175761-06:00'
model: gpt-4-1106-preview
summary: "Clojure-kieless\xE4 merkkijonon pituuden saa selville `count`-funktiolla."
title: "Merkkijonon pituuden selvitt\xE4minen"
weight: 7
---

## How to:
Clojure-kielessä merkkijonon pituuden saa selville `count`-funktiolla:
```Clojure
(count "Moi maailma!") ;=> 12
```

Mutta eipäs unohdeta `count`in ystävää - `length`-funktiota hänen Java isännässään:
```Clojure
(.length "Hyvää päivää!") ;=> 13
```

Kumpikin tapa antaa sinulle merkkijonon pituuden, digest-muodossa.

## Deep Dive
Clojure perustuu Javalle, joten `count` on käärö Java-metodille. Historiallisesti, laskuri-tyyppiset toiminnot heijastavat tietorakenteiden sisäisiä toteutuksia. Clojuressa `count` toimii eri tietotyypeillä, ei vain merkkijonoilla.

Vaihtoehtoja? Harvemmin tarvitaan, mutta rekursiivisia ja loop-recur -lähestymistapoja voi nähdä villissä luonnossa. Tässä esimerkki rekursiivisesta mallista merkkijonon pituuden laskemiselle:
```Clojure
(defn recursive-length [s]
  (if (empty? s)
    0
    (inc (recursive-length (subs s 1)))))
    
(recursive-length "Hei taas!") ;=> 9
```

Vaikkakin 'count' on tehokas ja suositeltu, on aina hyvä ymmärtää, miten voisi rakentaa ratkaisun alusta asti, jos se kerran tarvittaisiin.

## See Also
- Clojure `count` function: [ClojureDocs - count](https://clojuredocs.org/clojure.core/count)
- Java String `length()` method: [JavaDoc - String](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#length--)
