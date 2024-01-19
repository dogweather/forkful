---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Gleam: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Yhdistämällä merkkijonoja voimme liittää kaksi tai useampia merkkijonoja yhteen. Ohjelmoijille tämä on hyödyllistä, kun he haluavat esimerkiksi muodostaa dynaamisia merkkijonoja.

## Näin teet:
Clojure tukee merkkijonojen yhdistämistä `str` -funktion avulla.

```Clojure
(let [name "Maija" greeting "Hei"]
    (str greeting ", " name "."))
;; Outputtaa: "Hei, Maija."
```

## Syvä sukellus
Merkkijonon yhdistäminen on peräisin menneisyydestä, kun ohjelmoijien piti hallinnoida manuaalisesti muistia. Clojuren yhdistäminen käyttää Javan `StringBuilder` -luokkaa tehokkaiden ja muistitehokkaiden yhdisteiden luomiseksi.

Vaihtoehtona voit myös käyttää `StringBuilder` -luokkaa suoraan, jos tarvitset enemmän kontrollia tai tehokkuutta. 

Joissakin tapauksissa voit käyttää `format` -funktiota, kun haluat muotoilla merkkijonoja erityisellä tavalla.

```Clojure
(let [name "Pekka"]
    (format "Hei, %s." name))
;; Outputtaa: "Hei, Pekka."
```

## Katso myös
[Tutustu Clojuren viralliseen dokumentaatioon merkkijonoista.](https://clojure.org/guides/learn/strings)
[Javan StringBuilder dokumentaatio saatat myös olla hyödyllinen.](https://docs.oracle.com/javase/8/docs/api/java/lang/StringBuilder.html)