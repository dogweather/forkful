---
title:                "Merkkijonon suuraakkostaminen"
html_title:           "Clojure: Merkkijonon suuraakkostaminen"
simple_title:         "Merkkijonon suuraakkostaminen"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/clojure/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Merkkijonon suurella alkukirjaimella tarkoitetaan prosessia, jossa muutetaan merkkijonon ensimmäinen kirjain isoksi. Ohjelmoijat käyttävät tätä luomaan selkeämmän ja helposti luettavan koodin esim. koodin nimissä ja otsikoissa.

## Kuinka tehdään:

Clojure-ohjelmointikielessä voit suurentaa merkkijonon ensimmäisen kirjaimen käyttämällä `clojure.string/capitalize` -funktiota.

```Clojure 
(require '[clojure.string :as str])

(defn suuri-alkukirjain [s]
    (str/capitalize s))
```

Jos esimerkiksi annat syötteen "moi maailma", ohjelma antaa tuloksen "Moi maailma".

```Clojure
(suuri-alkukirjain "moi maailma")
```

## Syvällisemmin:

Merkkijonon ensimmäisen kirjaimen suurentaminen on ollut käytössä pitkään, sillä se parantaa merkittävästi koodin luettavuutta ja ymmärrystä. Koko merkkijonon muuttaminen isoksi ei ole usein käytännöllistä tai tarkoituksenmukaista, sillä se saattaa haitata tekstin luettavuutta.

Voit korjata merkkijonon jokaisen sanan suuren alkukirjaimen käyttämällä `clojure.string/capitalize` -funktiota yhdistettynä `clojure.string/split` -funktioon ja `clojure.string/join` -funktioon.

```Clojure
(defn suuri-alkukirjain-koko-merkkijono [s]
    (str/join " " (map str/capitalize (str/split s #" "))))
```

Tämä funktio jakaa merkkijonon sanoihin, suurentaa jokaisen sanan ensimmäisen kirjaimen ja liittää sanat takaisin yhteen.

## Katso myös:

Lisää tietoa merkkijonon käsittelystä Clojurella löydät näistä lähteistä:

- Clojuren virallinen dokumentaatio: [clojure.string](https://clojure.github.io/clojure/clojure.string-api.html)