---
title:    "Clojure: Merkkijonon pituuden löytäminen"
keywords: ["Clojure"]
---

{{< edit_this_page >}}

## Miksi

Miksi koodaaja haluaisi tietää merkkijonon pituuden? Merkkijonon pituuden laskeminen on hyödyllinen taito, joka auttaa koodaajaa hallitsemaan datan rakenteita ja suorittamaan tiettyjä toimintoja merkkijonoilla.

## Miten

Merkkijonon pituuden laskeminen Clojurella on helppoa ja yksinkertaista. Käytämme siihen `count` -funktiota, joka palauttaa merkkijonon pituuden. Katso alla oleva esimerkki:

```Clojure
(count "Tervetuloa Suomi")
```

Tämä koodi palauttaa 17, mikä on merkkijonon "Tervetuloa Suomi" pituus.

Voit myös laskea monen merkkijonon pituuden käyttämällä `count` -funktiota yhdistämällä merkkijonot listaksi.

```Clojure
(count (into [] "Moi" "hei" "terve"))
```

Koodi palauttaa 11, mikä on kaikkien kolmen merkkijonon yhteispituus.

## Syvällisempi sukellus

Vaikka `count` -funktio on tämän tehtävän kannalta riittävä, on hyvä tietää, että se laskiessaan merkkijonon pituutta se käyttää taustalla `seq` -funktiota. `Seq` -funktio laskee jokaisen merkin merkki-jonossa ja palauttaa sen pituuden. On myös hyvä huomata, että `seq` -funktio palauttaa `nil`, jos sitä sovelletaan tyhjään merkkijonoon.

## Katso myös

- [ClojureDocs: count](https://clojuredocs.org/clojure.core/count)
- [ClojureDocs: seq](https://clojuredocs.org/clojure.core/seq)