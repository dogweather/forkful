---
aliases:
- /fi/haskell/starting-a-new-project/
date: 2024-01-20 18:03:57.146098-07:00
description: "Aloitamme uuden projektin luomisen, koska meill\xE4 on visio tai ongelma,\
  \ johon etsimme ratkaisua. Haskellissa projekti tarkoittaa yleens\xE4 uuden paketin\
  \ tai\u2026"
lastmod: 2024-02-18 23:09:07.666901
model: gpt-4-1106-preview
summary: "Aloitamme uuden projektin luomisen, koska meill\xE4 on visio tai ongelma,\
  \ johon etsimme ratkaisua. Haskellissa projekti tarkoittaa yleens\xE4 uuden paketin\
  \ tai\u2026"
title: Uuden projektin aloittaminen
---

{{< edit_this_page >}}

## What & Why? – Mikä ja miksi?
Aloitamme uuden projektin luomisen, koska meillä on visio tai ongelma, johon etsimme ratkaisua. Haskellissa projekti tarkoittaa yleensä uuden paketin tai kirjaston kalustamista, missä koodataan järjestelmällisesti ja puhtaasti.

## How to: – Kuinka tehdään:
Aloita Stack-työkalulla. Se luo projektipohjan sinulle automaattisesti.

```Haskell
# Asenna Stack, jos sinulla ei sitä vielä ole
curl -sSL https://get.haskellstack.org/ | sh

# Luo uusi projekti
stack new esimerkkiprojekti

# Siirry projektiin ja kokeile kääntää se
cd esimerkkiprojekti
stack setup
stack build
```

Kun olet käynnistänyt `stack build`, voit nähdä esimerkiksi seuraavaa:

```
Building all executables for `esimerkkiprojekti` once. After a successful build of all of them, only specified executables will be rebuilt.
esimerkkiprojekti-0.1.0.0: configure (lib + exe)
...
esimerkkiprojekti-0.1.0.0: copy/register
Registering library for esimerkkiprojekti-0.1.0.0..
```

## Deep Dive – Syväsukellus:
Haskell-projekteihin on useita tapoja tarttua. Stack on nykyaikainen ja suosittu, mutta muitakin tapoja löytyy, kuten Cabal. Stack on kehitetty helpottamaan Haskell-kehitysympäristön kasaa, josta nimi tulee. Yksi Stackin eduista on, että se käsittelee itsenäisesti kaikki riippuvuudet sinun puolestasi ja varmistaa, että projekti toimii oikeassa ympäristössä oikeiden kirjastoversioiden kanssa.

2000-luvun alussa, Haskellin alustat ja työkalut olivat hajallaan, mutta Stackin ja sen pakettihallinnan myötä tilanne on parantunut huomattavasti. Stack käyttää päällä olevaa Cabal-työkalua pakettienhallintaan, mutta tarjoaa lisätoimintoja ja käyttäjäystävällisyyttä.

## See Also – Lisätietoja:
- [Stack's official documentation](https://docs.haskellstack.org/en/stable/README/)
- [Haskell's package repository Hackage](https://hackage.haskell.org/)
- [Stackage, a stable collection of Haskell packages](https://www.stackage.org/)
- [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/) - Hauska ja syvällinen johdatus Haskell-ohjelmointiin.
