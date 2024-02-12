---
title:                "Merkkijonosta lainausmerkkien poistaminen"
aliases:
- fi/haskell/removing-quotes-from-a-string.md
date:                  2024-01-26T03:40:02.380159-07:00
model:                 gpt-4-0125-preview
simple_title:         "Merkkijonosta lainausmerkkien poistaminen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?
Lainausmerkkien poistaminen merkkijonosta tarkoittaa yksittäisten (' ') tai kaksinkertaisten (" ") lainausmerkkien karsimista osana merkkijonotietoa. Ohjelmoijat tekevät sen puhdistaakseen syötteitä, valmistaakseen tekstiä käsittelyä varten tai poistaakseen tarpeettomia merkkejä, jotka saattaisivat häiritä datan käsittelyä ja toimintoja.

## Kuinka:
Haskellissa voimme luoda funktion, joka poistaa kaikki lainausmerkit annetusta merkkijonosta. Se on kuin sanoisimme lainausmerkeille, että häipykää, ja varmistaisimme, että ne ymmärtävät vihjeen.

```Haskell
import Data.List (intercalate)
import Data.Char (isPunctuation)

removeQuotes :: String -> String
removeQuotes = filter (\c -> c /= '"' && c /= '\'')

main :: IO ()
main = do
    let stringWithQuotes = "Haskell sanoi, \"Opiskellaanpa joitakin funktioita!\""
    putStrLn $ removeQuotes stringWithQuotes
```

Esimerkkitulostus:

```
Haskell sanoi, Opiskellaanpa joitakin funktioita!
```

## Syväsukellus
Olipa kerran aika, ennen kuin merkkijonot ohjelmoinnissa olivat yhtä yleisiä kuin kissavideot internetissä, tekstin käsittely oli hankalaa. Mutta kun ohjelmointikielet kehittyivät, merkkijonoista tuli olennainen osa koodaamista. Silti lainausmerkit säilyivät kaksiteräisenä miekkana – ne ovat olennaisia merkkijonojen määrittelyssä, mutta kiusankappaleina todellisina datoina.

Vaihtoehtoja? Sen sijaan, että huitaisisit kaikki lainausmerkit pois kuin kärpäset, voit olla valikoiva. Saatat haluta poistaa vain uloimmat lainausmerkit (klassinen trimmaus) tai käsitellä merkkijonon sisällä olevia paenneita lainausmerkkejä.

Toteutuksen osalta yllä oleva `removeQuotes`-funktio käyttää lambdalla jokaisen merkin (`c`) tarkistamista, onko se ärsyttävä lainausmerkki, ja suodattaa ne sen mukaisesti. Tämä on suoraviivainen lähestymistapa, mutta suurempien tekstien tai monimutkaisempien sääntöjen kohdalla saatat haluta tutkia jäsentäjäkirjastoja, kuten `Parsec`, joka voi tarjota enemmän hienostuneisuutta ja voimaa tekstinkäsittelyyn.

## Katso myös:
- Regex-rakastajille: [Text.Regex.Posix](https://hackage.haskell.org/package/regex-posix)
- Hellävarainen johdatus Haskellin merkkijonoihin: [Learn You a Haskell for Great Good! - Starting Out](http://learnyouahaskell.com/starting-out#strings)
