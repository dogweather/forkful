---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Haskell: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Regular expressions, eli säännölliset lausekkeet, ovat tapa etsiä tekstistä tiettyjä malleja. Ohjelmoijat käyttävät niitä tiedon suodatukseen, validointiin ja muokkaamiseen.

## Miten:
Voimme hakea tekstistä "klo" ja tämän perässä tulevat numerot seuraavasti:

```Clojure
(re-seq #"klo (\d+:\d+)" "klo 14:30, klo 15:00, klo 16:30")
;; => (["klo 14:30" "14:30"] ["klo 15:00" "15:00"] ["klo 16:30" "16:30"])
```
Säännöllinen lauseke `"klo (\d+:\d+)"` etsii jonoja, jotka alkavat merkkejä "klo ", jonka perässä on numerot pilkkuun asti. 

## Syvällämmälle:
Säännölliset lausekkeet lanseerattiin ensimmäisen kerran 1950-luvun lopulla matemaatikon, Stephen Kleenen, toimesta. Nykyisin monilla ohjelmointikielillä, mukaan lukien Clojure, on niiden tuki sisäänrakennettuna.

Vaihtoehtoisia tapoja tekstien muodostamiseen ja käsittelyyn ovat esim. merkkijono-funktiot ja tietorakenteet. Kuitenkin säännölliset lausekkeet tarjoavat usein tehokkaamman ja mukautuvamman ratkaisun.

Clojuren `re-seq`-funktio palauttaa sekvenssin otteluista annetussa merkkijonossa. Jos otteluja ei ole, it returns `nil`.

## Katso myös:
- [Clojuren säännöllisten lausekkeiden dokumentaatio](https://clojure.org/guides/learn/regex)
- [Regular-Expressions.info](https://www.regular-expressions.info/tutorial.html)
- [Säännöllisten lausekkeiden tutoriaali](https://regexone.com/)