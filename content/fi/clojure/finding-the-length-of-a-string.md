---
title:                "Merkkijonon pituuden selvittäminen"
date:                  2024-01-20T17:47:03.838360-07:00
model:                 gpt-4-1106-preview
simple_title:         "Merkkijonon pituuden selvittäminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/finding-the-length-of-a-string.md"
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
