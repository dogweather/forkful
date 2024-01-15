---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Clojure: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Oletko väsynyt manuaalisesti etsimään ja korvaamaan tekstiä tiedostoista? Haluatko säästää aikaa ja vaivaa? Clojure tarjoaa helpon tavan tehdä tämä automaattisesti, mikä tekee tekstien muokkaamisesta paljon nopeampaa ja tarkempaa.

## Miten tehdä se

Etsi ja korvaa toiminto Clojurella on helppo oppia ja käyttää. Käytä funktiota `replace`  tekstien etsimiseen ja korvaamiseen. Voit käyttää myös `replace` funktiota taulukoiden tai merkkijonojen kanssa. Katso alla oleva esimerkki:

```Clojure
(def teksti "Tämä on esimerkki tekstistä, jota haluat korvata.")
(replace teksti "haluat korvata" "haluat muuttaa")
```

Tuloksena on uusi merkkijono: "Tämä on esimerkki tekstistä, jota haluat muuttaa." Voit myös käyttää regex-merkkejä löytääksesi tiettyjä kuvioita tekstistä:

```Clojure
(def sana "tervehtimään")
(replace sana #"eh$" "aa")
```

Tämä korvaa "eh"-loppuiset sanat "aa"-loppuisilla sanoilla, jolloin tuloksena on merkkijono "tervehtimään". Löydät lisätietoja Clojuren `replace` funktion käytöstä [Clojuren dokumentaatiosta](https://clojuredocs.org/clojure.string/replace).

## Syvemmälle tekniseen

Clojuren `replace` toiminto käyttää `replace-first` ja `replace-all` funktioita, jotka puolestaan käyttävät Java-kirjaston `java.util.regex.Pattern` ja `java.util.regex.Matcher` luokkia. Tämä mahdollistaa monipuolisen ja tarkan tekstin etsimisen ja korvaamisen Clojurella. Voit löytää lisätietoja Java-kirjaston toiminnasta [Java:n virallisilta verkkosivuilta](https://docs.oracle.com/javase/7/docs/api/java/util/regex/Pattern.html).

## Katso myös

- [Clojuren viralliset verkkosivut](https://clojure.org/)
- [Etsi ja korvaa tekstiä Clojurella -opetusohjelma](https://www.braveclojure.com/finding-and-replacing-text-in-clojure/)