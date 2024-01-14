---
title:    "Clojure: Tekstin etsiminen ja korvaaminen"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi 
Tiedät varmasti sen tunteen, kun joudut manuaalisesti muuttamaan useita tekstidokumentteja samoilla muutoksilla. Onneksi Clojure tarjoaa helpon ja tehokkaan tavan automatisoida tekstihaun ja korvaamisen.

## Kuinka tehdä 
### Yksinkertainen haku ja korvaaminen
```Clojure
(def dokumentti "Tämä on esimerkkidokumentti, jossa esiintyy sana kissa.")
(clojure.string/replace dokumentti "kissa" "koira")
```

### Haku ja korvaaminen monilla vaihtoehdoilla
```Clojure
(def vaihtoehdot {"kissa" "koira", "linnut" "kaloja"})
(clojure.string/replace dokumentti #"kissa|linnut" vaihtoehdot)
```

### Regex-hakujen käyttäminen
```Clojure
(def dokumentti "Tämä on <esimerkki>dokumentista.</esimerkki>")
(def regex #"<.*/>")
(clojure.string/replace dokumentti regex "")
```

## Syväsukellus
Clojuren "clojure.string" kirjastossa on useita hyödyllisiä funktioita hakuun ja korvaamiseen. Voit esimerkiksi käyttää "split" ja "join" funktioita muuttaaksesi tekstin taulukoksi ja yhdistääksesi sen taas takaisin merkkijonoksi. Myös "re-find" ja "re-matcher" -funktiot voivat olla hyödyllisiä monimutkaisemmissa tilanteissa.

## Katso myös
- [Clojure 1.10.1 - String](https://clojuredocs.org/clojure.string)
- [Regular Expressions in Clojure](https://www.braveclojure.com/regular-expressions/)
- [Clojure Cookbook - Finding and Replacing Text](https://clojure-cookbook.com/finding-replacing-text/index.html)