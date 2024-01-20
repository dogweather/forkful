---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Arduino: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Tekstin hakeminen ja korvaaminen on ohjelmoinnillinen toimenpide, jossa etsitään ja vaihdetaan merkkijonon osia toisiin. Ohjelmoijat tekevät tämän tiedon muotoilun helpottamiseksi ja tehokkuuden parantamiseksi.

## Miten?
Clojuressa voimme käyttää `clojure.string/replace` funktiota tekstin korvaamiseen. Katso esimerkki alla:

```Clojure
(require '[clojure.string :as str])

(def original-text "Hei, olen ohjelmoija!")
(def modified-text (str/replace original-text "olen" "AI"))

(println modified-text)
```

Kun suoritat koodin, saat alla olevan tulostuksen:

```Clojure
"Hei, AI ohjelmoija!"
```

## Syvempi tieto
Historiallisesti tekstinkäsittelyn työkalut kuten SED ja AWK sisälsivät jo tekstinkorvausominaisuudet. Niiden rajoituksista johtuen syntyi uusia, monipuolisempia kirjastoja kuten Perl ja nyt Clojure.

Clojuressa on vaihtoehtoja tekstinkorvauksen suorittamiseen, kuten käyttämällä regular expression (regex). `str/replace` funktio mahdollistaa regexin käytön, mikä tekee siitä erittäin joustavan työkalun.

`str/replace`-toteutuksen yksityiskohdat paljastavat, että funktio käyttää Java String-luokan `replace`-metodia. Tämä tekee toiminnosta erittäin suorituskykyisen, koska se hyödyntää suoraan Javan vakioita.

## Katso myös
1. Clojure officiaalinen dokumentaatio: https://clojure.org/guides/getting_started
2. Clojure string manipuloinnin opas: https://clojure.org/api/cheatsheet
3. Regex in Clojure: https://www.regular-expressions.info/clojure.html