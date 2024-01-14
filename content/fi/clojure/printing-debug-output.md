---
title:    "Clojure: Tulostaminen virheenkorjauksessa"
keywords: ["Clojure"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Miksi tulostuksen vianetsinnästä saattaisi olla hyötyä Clojure-ohjelman tekijöille? 

Vianetsintä on tärkeä osa ohjelmointia, ja tulostuksen avulla voimme tarkastella ohjelman kulkua ja mahdollisia virheitä.

## Kuinka
```Clojure
(defn calculate [a b]
  (+ a (inc b)))
 
(def x 5)
(def y 10)
 
(println "Laskemme x:n ja y:n summan...")
(println "Lopputulos: " (calculate x y))
```
Tämä koodi näyttää yksinkertaisen esimerkin tulostuksen käytöstä ohjelman virheiden etsimisessä. Aloitamme määrittelemällä calculate-funktion, joka laskee kahden luvun summan. sitten luomme kaksi muuttujaa, x ja y, joiden arvot ovat 5 ja 10. Lopuksi tulostamme lasketun summan konsoliin. Näemme, että laskettu arvo on 16, mikä tarkoittaa, että kaava ei toimi odotetulla tavalla. Käyttämällä tulostusta voimme paikantaa virheen ja korjata koodin.

## Syvemmälle
Tulostuksen vianetsinnän hyödyt eivät rajoitu vain virheiden paikantamiseen. Voimme myös käyttää sitä tarkastellaksemme ohjelman suoritusjärjestystä ja varmistamaan, että laskemamme arvot ovat oikein. Lisäksi jos meillä on laajempi ohjelma, voimme käyttää eri tasoja tulostuksen avulla ja tarkastella tiettyjen osien suoritusta.

## Katso myös
- [Tulostuksen käyttö Clojure-koodissa](https://web.archive.org/web/20110420124717/http://doing-fp-right.blogspot.com/2009/01/using-prinln-in-clojure-program.html)
- [Debuggaus Clojurella](https://clojure.org/guides/debugging)
- [Clojure-debuggaus Githubissa](https://github.com/clojure-goes-fast/clojure-debugging)