---
title:                "Tekstin etsiminen ja korvaaminen"
html_title:           "Elm: Tekstin etsiminen ja korvaaminen"
simple_title:         "Tekstin etsiminen ja korvaaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Miksi

Joskus ohjelmointiprojektissasi voi olla tarve etsiä tiettyä tekstiä koodistasi ja korvata se toisella tekstillä. Tämä voi johtua esimerkiksi vanhanaikaisten funktioiden tai muuttujien nimeämiskäytäntöjen päivittämisestä tai yksinkertaisesti virheellisen tekstikohdan korjaamisesta.

## Miten

Voit käyttää Elm:n String-moduulin tarjoamia funktioita tekstinkäsittelyyn. Alla on esimerkki, jossa korvaamme kaikki "hello" sanat "hei".

```Elm
import String exposing (replace)

sampleString = "Hello world!"

sampleString |> replace "hello" "hei" |> toString -- output: "Hei world!"
```

Voit myös käyttää tarkempaa haku-toimintoa käyttämällä `Regex.replace` -funktiota. Tässä esimerkissä korvaamme kaikki sanat, jotka alkavat kirjaimilla "ha" ja loppuvat vokaaliin, sanalla "hauska".

```Elm
import Regex exposing (replace)

sampleString = "Olen hajamielinen käyttäjä."

sampleString |> replace (Regex.regex "(ha[a-z])*a") (\_ -> "hauska") |> toString -- output: "Olen hauska käyttäjä."
```

## Syventyvä tarkastelu

Mikäli haluat tutustua tarkemmin String-moduulin ja Regex-funktioiden käyttöön tekstinkäsittelyssä, voit lukea [virallisen dokumentaation](https://package.elm-lang.org/packages/elm/core/latest/String) tai tehdä harjoituksia [Elm:tut:lla](https://tut.elm-lang.org/). 

## Katso myös

- [String-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm:tut](https://tut.elm-lang.org/)