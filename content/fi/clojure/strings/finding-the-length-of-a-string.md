---
date: 2024-01-20 17:47:03.838360-07:00
description: "Mittaamme merkkijonojen pituutta, koska haluamme selvitt\xE4\xE4, kuinka\
  \ monta merkki\xE4 ne sis\xE4lt\xE4v\xE4t tai validoida niiden sis\xE4lt\xF6\xE4\
  . T\xE4m\xE4 auttaa mm. l\xF6yt\xE4m\xE4\xE4n\u2026"
lastmod: '2024-03-11T00:14:30.104025-06:00'
model: gpt-4-1106-preview
summary: "Mittaamme merkkijonojen pituutta, koska haluamme selvitt\xE4\xE4, kuinka\
  \ monta merkki\xE4 ne sis\xE4lt\xE4v\xE4t tai validoida niiden sis\xE4lt\xF6\xE4\
  . T\xE4m\xE4 auttaa mm. l\xF6yt\xE4m\xE4\xE4n\u2026"
title: "Merkkijonon pituuden selvitt\xE4minen"
---

{{< edit_this_page >}}

## What & Why?
Mittaamme merkkijonojen pituutta, koska haluamme selvittää, kuinka monta merkkiä ne sisältävät tai validoida niiden sisältöä. Tämä auttaa mm. löytämään virheitä tai rajoittamaan syötteen kokoa.

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
