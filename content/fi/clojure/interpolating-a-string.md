---
title:                "Merkkijonon interpolointi"
html_title:           "Clojure: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/interpolating-a-string.md"
---

{{< edit_this_page >}}

##### Mitä & Miksi? 
Interpolointi tarkoittaa merkkijonon sisäisten muuttujien korvaamista. Sitä käytetään usein ohjelmoinnissa, kun haluamme luoda dynaamisia tai muuttuvia merkkijonoja. 

##### Miten:
Esimerkiksi, jos haluat tulostaa käyttäjän antaman nimen tervehdyksen kanssa, voit käyttää interpolointia merkkijonossa: ```Clojure
"Teretuloa, $name!"```

Tässä ```$name``` muuttuja korvataan käyttäjän antamalla nimellä. Lopputulos olisi esimerkiksi ```Clojure
"Teretuloa, Johanna!"```

Tämä on paljon kätevämpää kuin yrittää yhdistää muuttujia ja tekstiä erikseen.

##### Syvemmälle:
Interpoloinnin käsite on peräisin Perl-ohjelmointikielestä. Sitä käytetään myös monissa muissa ohjelmointikielissä, kuten Rubyssa ja Pythonissa. Jotkut Clojuren ohjelmoijat käyttävät mieluummin funktioita, kuten ```str``` ja ```format```, interpoloinnin sijaan.

##### Katso myös:
Voit lukea lisää interpoloinnista Clojuren virallisesta dokumentaatiosta: https://clojure.org/guides/string_conversion

Voit myös tutustua muiden ohjelmointikielien interpolointitapoihin, kuten Rubyn ja Pythonin f-stringeihin.