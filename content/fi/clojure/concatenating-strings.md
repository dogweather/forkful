---
title:                "Merkkijonojen yhdistäminen"
html_title:           "Clojure: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/concatenating-strings.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Jonojen yhdistäminen on tapa yhdistää kaksi tai useampaa merkkijonoa yhdeksi. Tämä on hyödyllistä esimerkiksi silloin, kun haluat yhdistää nimet ja sukunimet yhdeksi kokonaisuudeksi. Ohjelmoijat käyttävät jonojen yhdistämistä lähinnä tiedon esittämiseen ja käsittelyyn.

## Kuinka tehdä?
```Clojure
(def etunimi "Mikko")
(def sukunimi "Virtanen")
(str etunimi " " sukunimi) ; "Mikko Virtanen"
```

Voit myös yhdistää useampia merkkijonoja samassa lauseessa:

```Clojure
(str "Hei " etunimi " " sukunimi "!") ; "Hei Mikko Virtanen!"
```

## Syväsukellus
Jonojen yhdistäminen on yleinen käytäntö monissa ohjelmointikielissä. Joissakin kielissä käytetään erillisiä operaattoreita, kuten "+" tai "&", kun taas Clojuressa käytetään funktiota "str". Myös muutamat sisäänrakennetut tietotyypit, kuten listat ja vektorit, voidaan yhdistää käyttämällä funktiota "into".

Mikäli haluat yhdistää suuren määrän merkkijonoja, voi olla tehokkaampaa käyttää Clojuressa sisäänrakennettua "StringBuilder" luokkaa.

## Katso myös
Lisää tietoa jonojen yhdistämisestä löydät [Clojure-dokumentaatiosta](https://clojure.org/reference/strings). Voit myös tutustua muihin tapoihin manipuloida merkkijonoja Clojuressa [tästä artikkelista](https://clojurefundamentals.com/10-manipulating-strings-in-clojure/).