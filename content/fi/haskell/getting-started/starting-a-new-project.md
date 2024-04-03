---
date: 2024-01-20 18:03:57.146098-07:00
description: "How to: \u2013 Kuinka tehd\xE4\xE4n: Aloita Stack-ty\xF6kalulla. Se\
  \ luo projektipohjan sinulle automaattisesti."
lastmod: '2024-03-13T22:44:56.615895-06:00'
model: gpt-4-1106-preview
summary: "Aloita Stack-ty\xF6kalulla."
title: Uuden projektin aloittaminen
weight: 1
---

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
