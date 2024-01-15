---
title:                "Alimerkkien erottaminen"
html_title:           "Clojure: Alimerkkien erottaminen"
simple_title:         "Alimerkkien erottaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/extracting-substrings.md"
---

{{< edit_this_page >}}

## Miksi

Joskus haluat ehkä irrottaa osan jonkin merkkijonon sisällöstä. Tämä voi olla hyödyllistä esimerkiksi käsiteltäessä tekstikäyttöliittymiä tai tietokantoja.

## Miten

Voit käyttää `subs` -funktiota irrottamaan alimerkkijonoja merkkijonosta. Se ottaa kaksi argumenttia: merkkijonon ja aloituskohdan, josta alkaen haluat leikata merkkijonoa. Voit myös määrittää valinnaisen kolmannen argumentin, joka kertoo kuinka monta merkkiä haluat ottaa mukaan. Esimerkiksi:

```Clojure
(defn extract-substring [string start end]
    (subs string start end))

(extract-substring "Hei maailma!" 4 8)
```
Tulostus:
```
"maai"
```

## Syventävä sukellus

`subs` -funktion lisäksi Clojurella on muita tapoja irrottaa alimerkkijonoja. Voit esimerkiksi käyttää `split-at` -funktiota jakamaan merkkijonon haluamastasi kohdasta kahteen osaan. Voit myös käyttää säännöllisiä lausekkeita `re-seq` -funktioon, joka palauttaa listan merkkijonoista, jotka täsmäävät annettuun lausekkeeseen.

## Katso myös

- [Clojure:n dokumentaatio merkkijonofunktioista] (https://clojure.org/reference/strings)
- [Clojure Command Line Ref - iniciaragge] (https://clojure.org/guides/getting_started_cmdline)