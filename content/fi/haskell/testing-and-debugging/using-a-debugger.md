---
date: 2024-01-26 03:50:13.140023-07:00
description: "Debuggerin k\xE4ytt\xE4minen tarkoittaa koodisi syv\xE4llist\xE4 tutkimista\
  \ ty\xF6kaluilla, jotka on suunniteltu tarkastelemaan, keskeytt\xE4m\xE4\xE4n ja\
  \ manipuloimaan ohjelmaa\u2026"
lastmod: '2024-03-13T22:44:56.619545-06:00'
model: gpt-4-0125-preview
summary: "Debuggerin k\xE4ytt\xE4minen tarkoittaa koodisi syv\xE4llist\xE4 tutkimista\
  \ ty\xF6kaluilla, jotka on suunniteltu tarkastelemaan, keskeytt\xE4m\xE4\xE4n ja\
  \ manipuloimaan ohjelmaa\u2026"
title: "Debuggerin k\xE4ytt\xF6"
---

## Kuinka:
Kävellään läpi GHCi:n kanssa, Haskellin interaktiivisen ympäristön, joka voi toimia perusdebuggerina. Käynnistät sen Haskell-koodisi kanssa ja alat tutkia sitä. Tässä on esimerkki:

```Haskell
main :: IO ()
main = do
    putStrLn "Hei, mikä on nimesi?"
    nimi <- getLine
    putStrLn $ "Hei, " ++ nimi ++ "! Aletaan debugata."
    let tulos = viallinenFunktio 5
    print tulos

viallinenFunktio :: Int -> Int
viallinenFunktio n = n * 2 -- Kuvittele, että tässä on bugi
```

Aloittaaksesi debuggauksen GHCi:n kanssa:

```bash
$ ghci YourHaskellFile.hs
```

Aseta katkaisukohta `viallinenFunktio`-funktioon:

```Haskell
Prelude> :break viallinenFunktio
```

Suorita ohjelmasi:

```Haskell
Prelude> :main
Hei, mikä on nimesi?
```

Ohjelmasi keskeytyy `viallinenFunktio`-kohtaan. Nyt voit tarkastella muuttujia, seurata koodia askel askeleelta ja arvioida lausekkeita.

## Syväsukellus:
Historiallisesti Haskellin maine puhtaiden funktioiden ja vahvan tyypityksen ansiosta johti uskomukseen, että debuggaustyökalut olivat vähemmän kriittisiä. Todellisuus on toinen—monimutkaiset ohjelmat hyötyvät aina hyvistä debuggaustyökaluista. GHCi tarjoaa perus debuggauskomentoja. Kuitenkin, visuaalisempaa kokemusta tai laajamittaisempien sovellusten varten saatat tutkia IDE:tä, jolla on integroidut debuggerit, kuten Visual Studio Code Haskell-laajennuksilla tai IntelliJ:n Haskell-liitännäisellä.

Vaihtoehtoja debuggerille ovat print-lauseiden käyttö, tunnetaan nimellä "printf-debuggaus", tai hyödyntäen Haskellin vahvaa tyypitysjärjestelmää tekemään virheelliset tilat esittämättömiksi. Silti, joskus koodin läpi kulkeminen korvaamattomana.

Mitä toteutuksen yksityiskohtiin tulee, Haskellin debuggeri toimii suoritusaikajärjestelmän kanssa. Se voi käsitellä katkaisukohtia, askel suoritusta ja sallia muuttujatarkastelun. Kuitenkin, koska Haskell on laiskasti arvioitu, asiat voivat olla hieman epäintuitiivisia. Haskell-ohjelman debuggaus tarkoittaa usein silmälläpitämistä siitä, milloin ja miten lausekkeet arvioidaan.

## Katso myös:
- [GHC:n käyttäjän opas - Debugger](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/debugging.html)
- [IntelliJ Haskell -liitännäinen](https://plugins.jetbrains.com/plugin/8258-intellij-haskell)
