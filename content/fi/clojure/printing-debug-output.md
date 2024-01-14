---
title:                "Clojure: Tulostamassa virheenjäljitystulosteita"
simple_title:         "Tulostamassa virheenjäljitystulosteita"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi

Printtaaminen tahnaustulosteita on tärkeä osa Clojure-ohjelmoinnin kehittämistä ja virheiden korjaamista. Se auttaa kehittäjiä ymmärtämään, mitä koodi todella tekee ja missä mahdolliset virheet voivat olla.

## Miten

Useimmat Clojure-kehittäjät käyttävät `println`-funktiota tahnaustulosteiden printtaamiseen. Tämä funktio hyväksyy minkä tahansa arvon ja tulostaa sen konsoliin. Alla on esimerkki, jossa printataan muuttujan `nimi` arvo konsoliin:

```Clojure
(def nimi "Jesse")
(println nimi)
```

Tämä tulostaa `Jesse` konsoliin. Voit myös ketjuttaa useita arvoja yhdessä `println`-funktion kanssa käyttämällä `str`-funktiota. Alla on esimerkki printtaamisesta useammalla arvolla:

```Clojure
(def luku1 10)
(def luku2 5)

(println (str "Ensimmäinen luku: " luku1 ", toinen luku: " luku2))
```

Tämä tulostaa `Ensimmäinen luku: 10, toinen luku: 5` konsoliin.

## Syvemmälle

Tahnaustulosteen printtaaminen voi auttaa kehittäjiä myös muokkaamaan ja tarkistamaan koodia. Voit käyttää `prn`-funktiota, joka toimii samalla tavalla kuin `println`, mutta tulostaa myös koodin lukuarvon. Tämä voi auttaa sinua ymmärtämään paremmin koodin suoritusjärjestystä ja mahdollisia virheitä.

Jos haluat tarkistaa, onko jokin ehto totta tai epätotta, voit käyttää `println`-funktiota yhdistettynä `if`-lauseeseen. Alla on esimerkki:

```Clojure
(let [luku 5]
  (if (> luku 10)
    (println "Luku on suurempi kuin 10")
    (println "Luku on pienempi tai yhtä suuri kuin 10")))
```

Tämä tulostaa `Luku on pienempi tai yhtä suuri kuin 10`, koska ehto `luku > 10` ei ole totta.

## Katso myös

- [Clojure-line editor](https://github.com/clojure-emacs/clojure-mode)
- [Clojure-kirjoittajan opas](https://clojure.org/guides/learn/guides-for-professional-programmers)