---
title:    "Haskell: Virheenetsintätulostus"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## Miksi

Debug tiedon tulostaminen on tärkeä osa Haskell ohjelmointia, sillä se auttaa havaitsemaan ja korjaamaan virheitä ohjelman suorituksen aikana. Se antaa myös ohjelmoijalle paremman ymmärryksen siitä, miten ohjelma toimii ja mitä vaiheita se suorittaa.

## Miten

```Haskell
main :: IO ()
main = do
    putStrLn "Aloita debug tiedon tulostaminen"
    putStr "Arvot listassa: "
    print [1, 2, 3]
```

Tämä yksinkertainen esimerkki näyttää miten voidaan tulostaa viesti "Aloita debug tiedon tulostaminen" sekä lista [1, 2, 3]. Huomaa, että käytämme funktiota print sen sijaan, että tulostaisimme suoraan käyttäen putStrLn funktiota. Tämä johtuu siitä, että print funktio ottaa vastaan minkä tahansa tietotyypin ja muuttaa sen merkkijonoksi, jolloin voimme tulostaa myös esimerkiksi listoja tai tupleja.

```Haskell
debugFunktio :: [Int] -> String
debugFunktio [] = "Tyhjä lista"
debugFunktio (x:xs) = "Listan ensimmäinen alkio on " ++ show x

main :: IO ()
main = do
    let lista = [1, 2, 3]
    putStrLn "Debug tietoa listan alkiosta:"
    putStrLn (debugFunktio lista)
```

Tässä toisessa esimerkissä näytetään, miten voimme luoda oman debug funktion, joka ottaa vastaan listan kokonaislukuja ja palauttaa merkkijonon sen perusteella. Käytämme funktiota show, joka muuttaa arvon merkkijonoksi, jotta voimme yhdistää sen ++ operaattorilla.

## Syventävä sukellus

Debug tiedon tulostaminen voi olla myös hyödyllistä silloin, kun haluamme nähdä tarkemmin, mitä funktiot tekevät ja miten ohjelman suoritus etenee. Esimerkiksi voimme käyttää Debug.Trace moduulia, joka tarjoaa useita hyödyllisiä funktioita debug tiedon tulostamiseen.

```Haskell
import Debug.Trace

debugFunktio :: [Int] -> String
debugFunktio [] = trace "Tyhjä lista" "Tyhjä lista"
debugFunktio (x:xs) = trace ("Listan ensimmäinen alkio on " ++ show x) ("Listan ensimmäinen alkio on " ++ show x)

main :: IO ()
main = do
    let lista = [1, 2, 3]
    putStrLn "Debug tietoa listan alkiosta:"
    putStrLn (debugFunktio lista)
```

Trace funktiot ottavat vastaan merkkijonon ja arvon ja tulostavat ne, mutta myös palauttavat arvon, jotta voimme käyttää niitä esimerkiksi funktioiden välisessä kommunikaatiossa. Tässä esimerkissä näemme, miten voimme käyttää trace funktioita lisäämään debug tietoa omaan koodiin.

## Katso myös

- [Haskell Debug.Trace dokumentaatio](https://hackage.haskell.org/package/base-4.15.0.0/docs/Debug-Trace.html)
- [Debugging with Haskell in VS Code](https://blog.adamharpur.com/devenv/2019/06/04/haskell-debugging-with-visual-studio-code/)
- [Debugging Haskell Code with GHCi](https://codecrafters.io/blog/haskell-gdb-debugging/)