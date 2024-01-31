---
title:                "Säännöllisten lausekkeiden käyttö"
date:                  2024-01-19
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"

category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Säännölliset lausekkeet ovat haku- ja korvausoperaatioihin käytettyjä tekstipätkiä. Ohjelmoijat käyttävät niitä tekstidatan tehokkaaseen prosessointiin ja validointiin.

## How to:
```Clojure
; Stringiin sopimisen tarkistus
(re-matches #"\b[Cc]lojure\b" "I love Clojure!")
; => "Clojure"

; Etsi osuvat osajonot
(re-seq #"\b\w+" "Clojure, Java & Haskell")
; => ("Clojure" "Java" "Haskell")

; Korvaa sopivat osuudet
(clojure.string/replace "Clojure on hauskaa!" #"[aou]" "i")
; => "Clijiire in haiskii!"
```

## Deep Dive
Säännöllisiä lausekkeita kehiteltiin alun perin 1950-luvulla automaattisten kielentunnistajien yhteydessä. Perl kielen popularisoinnin myötä ne levisivät laajempaan käyttöön. Clojuressa, kuten muissakin moderneissa kielissä, säännölliset lausekkeet toimivat osana standardikirjastoa. Perl-tyylisten regex-moottorien ohella olemassa on myös muiden tyyppisiä, kuten POSIX. Tietyissä tapauksissa tekstinkäsittelyn voi tehdä myös ilman säännöllisiä lausekkeita, esimerkiksi käyttämällä merkkijonojen omaa `split`, `join` tai `contains?` funktioita.

## See Also
- [ClojureDocs Regular Expressions](https://clojuredocs.org/clojure.core/re-matches)
- [Clojure: java.util.regex](https://clojure.org/reference/java_interop#_java_util_regex)
- [Java Pattern class documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/util/regex/Pattern.html)
