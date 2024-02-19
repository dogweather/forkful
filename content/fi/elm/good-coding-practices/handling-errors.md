---
aliases:
- /fi/elm/handling-errors/
date: 2024-01-26 00:51:19.600762-07:00
description: "Virheiden k\xE4sittely tarkoittaa koodin kirjoittamista, joka osaa ennakoida\
  \ ja k\xE4sitell\xE4 ongelmia. Ohjelmoijat tekev\xE4t sit\xE4 v\xE4ltt\xE4\xE4kseen\
  \ kaatumisia,\u2026"
lastmod: 2024-02-18 23:09:07.536899
model: gpt-4-1106-preview
summary: "Virheiden k\xE4sittely tarkoittaa koodin kirjoittamista, joka osaa ennakoida\
  \ ja k\xE4sitell\xE4 ongelmia. Ohjelmoijat tekev\xE4t sit\xE4 v\xE4ltt\xE4\xE4kseen\
  \ kaatumisia,\u2026"
title: "Virheiden k\xE4sittely"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Virheiden käsittely tarkoittaa koodin kirjoittamista, joka osaa ennakoida ja käsitellä ongelmia. Ohjelmoijat tekevät sitä välttääkseen kaatumisia, suojatakseen tietojen eheyttä ja tarjotakseen käyttäjille sujuvia varasuunnitelmia.

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
