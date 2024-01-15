---
title:                "Merkkijonojen poistaminen vastaavien kuvioiden perusteella."
html_title:           "Haskell: Merkkijonojen poistaminen vastaavien kuvioiden perusteella."
simple_title:         "Merkkijonojen poistaminen vastaavien kuvioiden perusteella."
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi

Oletko koskaan kohdannut tilanteen, jossa haluat poistaa tiettyjä merkkejä tekstistäsi? Ehkä haluat puhdistaa sisältöäsi tai filtteröidä tiettyjä sanoja. Kannattaa harkita Haskellin käyttöä tähän tehtävään.

## Kuinka

Näin poistat merkkejä vastaavat kaavan mukaiset merkit tekstistäsi:

```Haskell
delete :: Eq a => [a] -> [a] -> [a]
delete _ [] = []
delete [] ys = ys
delete (x:xs) ys = delete xs (remove x ys)
	where
		remove :: Eq a => a -> [a] -> [a]
		remove _ [] = []
		remove x (y:ys) | x == y = remove x ys
				| otherwise = y : remove x ys
```

Esimerkkituloste:

```Haskell
delete "a" "kissa" = "kiss"
delete "aa" "kaakko" = "kko"
```

## Syvemmällä

Tämä ratkaisu käyttää hyödyksi Haskellin tarjoamaa `delete`-funktiota, joka poistaa ensimmäisen esiintymän listassa. `remove`-funktio puolestaan käy läpi listan ja poistaa kaikki merkit, jotka vastaavat kaavaa.

`delete`-funktio ottaa vastaan kaksi listaa, ensimmäinen on merkit, jotka haluat poistaa ja toinen on tekstisi. Se kutsuu `remove`-funktiota jokaiselle ensimmäisen listan merkille ja palauttaa lopulta puhdistetun tekstin.

Jos haluat poistaa kaikki tietyn kaavan mukaiset merkit tekstistäsi, voit käyttää tätä funktiota yhdistettynä esimerkiksi `filter`-funktioon. Voit myös laajentaa tätä ratkaisua siten, että se poistaa esimerkiksi kaikki välimerkit tai isot kirjaimet.

## Katso myös

- [Haskellin dokumentaatio](https://www.haskell.org/documentation/)
- [Haskellin opetusohjelmat](https://wiki.haskell.org/Tutorials)
- [Haskellin virallinen sivusto](https://www.haskell.org/)