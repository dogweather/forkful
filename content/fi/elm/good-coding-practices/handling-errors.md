---
date: 2024-01-26 00:51:19.600762-07:00
description: "Kuinka: Elmin perusfilosofia on Ei Suoritusaikaisia Poikkeuksia. Joten\
  \ Elm hy\xF6dynt\xE4\xE4 tyypitysj\xE4rjestelm\xE4\xE4ns\xE4 tyypeill\xE4 kuten\
  \ `Maybe` ja `Result` virheiden\u2026"
lastmod: '2024-03-13T22:44:56.496113-06:00'
model: gpt-4-1106-preview
summary: Elmin perusfilosofia on Ei Suoritusaikaisia Poikkeuksia.
title: "Virheiden k\xE4sittely"
weight: 16
---

## Kuinka:
Elmin perusfilosofia on Ei Suoritusaikaisia Poikkeuksia. Joten Elm hyödyntää tyypitysjärjestelmäänsä tyypeillä kuten `Maybe` ja `Result` virheiden käsittelyyn.

`Maybe` skenaariolle:

```Elm
safeDivide : Float -> Float -> Maybe Float
safeDivide numerator denominator =
    if denominator == 0 then
        Nothing
    else
        Just (numerator / denominator)
        
-- Kun ajat sen:

safeDivide 10 2
--> Just 5

safeDivide 10 0
--> Nothing
```

`Result` skenaariolle:

```Elm
type Error = DivisionByZero

safeDivide : Float -> Float -> Result Error Float
safeDivide numerator denominator =
    if denominator == 0 then
        Err DivisionByZero
    else
        Ok (numerator / denominator)

-- Ja käyttäen sitä:

safeDivide 10 2
--> Ok 5

safeDivide 10 0
--> Err DivisionByZero
```

## Syväsukellus
Elmin tyypitysjärjestelmä on tiukka, mikä auttaa löytämään virheet aikaisin. Historiallisesti useimmat kielet ovat luottaneet poikkeuksiin ja suoritusaikaisiin tarkistuksiin, mutta Elm valitsi käännösaikaiset takeet. Vaihtoehdot kuten `Result` mahdollistavat yksityiskohtaisen virhetiedon, kun taas `Maybe` on yksinkertaisempi kyllä-ei-skenaarioissa. Elmin virheenkäsittely kannustaa kehittäjiä harkitsemaan kaikkia polkuja etukäteen, välttäen näin unohtuneiden virhetapausten sudenkuopat.

## Katso myös:
- Elmin virallinen oppaan osio virheenkäsittelystä: [Virheenkäsittely – Johdanto](https://guide.elm-lang.org/error_handling/)
- Elm `Maybe` dokumentaatio: [Elm – Maybe](https://package.elm-lang.org/packages/elm/core/latest/Maybe)
- Elm `Result` dokumentaatio: [Elm – Result](https://package.elm-lang.org/packages/elm/core/latest/Result)
