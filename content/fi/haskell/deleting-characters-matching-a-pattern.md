---
title:                "Haskell: Mallia vastaavien merkkien poistaminen"
programming_language: "Haskell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/haskell/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Miksi 

Kuvittele, että sinulla on valtava määrä tietoa tai tekstiä, josta haluat poistaa tietyt merkit tai merkkijonot. Ehkä haluat vain poistaa ylimääräiset välilyönnit tai erikoismerkit, tai ehkä sinun täytyy suodattaa tietoa tietystä formaatista. Tässä tilanteessa on äärimmäisen tärkeää löytää tapa poistaa nämä merkit nopeasti ja tehokkaasti. Tässä tulee apuun haskellin ominaisuus poistaa merkkejä, jotka täyttävät tietyn kuvion.

## Kuinka Teet

Haskellilla on käytännöllinen tapa poistaa merkkejä, jotka ovat dynaamisesti määritettyjä merkkijonoja. Voit käyttää funktiota "deleteChar" poistaaksesi merkkejä merkkijonosta, joka vastaa tiettyä kuvioa. Voit tehdä tämän esimerkiksi seuraavalla tavalla:

```Haskell
deleteChar :: Char -> String -> String
deleteChar c [] = []
deleteChar c (x:xs) | c == x    = deleteChar c xs
                    | otherwise = x : deleteChar c xs
```

Tämä esimerkki funktio ottaa parametreina merkin ja merkkijonon ja palauttaa uuden merkkijonon, jossa merkki on poistettu. Voit kutsua tätä funktiota ja antaa parametriksi haluamasi merkin ja merkkijonon, josta haluat poistaa merkin. Esimerkiksi:

```Haskell
deleteChar 'a' "Haskell on mahtava!"  #=> "Hskell on mhtv!"
```

Tässä tapauksessa välilyönnit eivät ole välitettyjä, joten ne eivät ole tuloksessa.

## Syväsukellus

Kun käytät "deleteChar" -funktiota haskellissa, sinun täytyy pitää mielessä, että se toimii rekursiivisesti. Tämä tarkoittaa sitä, että funktio kutsuu itseään, kunnes tulee tietty ehto, jolloin se palauttaa halutun tuloksen.

Funktio ei myöskään muuta alkuperäistä merkkijonoa, vaan palauttaa uuden merkkijonon, joka sisältää muutokset. Tämä on tärkeää pitää mielessä, kun käytät "deleteChar" -funktiota osana laajempaa ohjelmaa.

## Katso myös

- [Haskelin viralliset kotisivut] (https://www.haskell.org/)
- [Tutorial: Haskel on 5 minuutissa] (https://www.haskell.org/downloads/#platform)
- [Haskel Cheat Sheet] (https://haskel.org/haskell-cheat-sheet/)