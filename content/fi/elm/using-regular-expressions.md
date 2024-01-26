---
title:                "Säännöllisten lausekkeiden käyttö"
html_title:           "Bash: Säännöllisten lausekkeiden käyttö"
simple_title:         "Säännöllisten lausekkeiden käyttö"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Säännölliset lausekkeet ovat hakumalleja tekstissä tunnistamiseen. Käytämme niitä, koska ne tekevät tekstin prosessoinnista nopeaa ja joustavaa.

## How to:
Elm käyttää `Regex`-pakettia säännöllisten lausekkeiden käsittelyyn. Tässä helppo esimerkki:

```Elm
import Regex

-- Säännöllisen lausekkeen luominen
emailRegex : Regex.Regex
emailRegex =
    Regex.fromString "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}" |> Maybe.withDefault Regex.never

-- Tekstin hakeminen säännöllisen lausekkeen avulla
isEmailValid : String -> Bool
isEmailValid email =
    Regex.contains emailRegex email

-- Käytön esimerkki
main =
    Html.text (String.fromBool (isEmailValid "esimerkki@domain.fi"))
```

Tämän pitäisi tulostaa `True` jos sähköposti on kelvollinen.

## Deep Dive
Säännöllisiä lausekkeita on hyödynnetty jo 1950-luvulta lähtien. Elm ei ole yhtä suoraviivainen regexien kanssa kuin jotkin muut kielet. Esimerkiksi `JavaScript` on perinteisesti ollut regex-vahva. Elm käyttää natiivia `Regex`-kirjastoa, ja regexien käsittely tapahtuu puhtaasti funktionaalisesti, mikä on Elmin periaatteiden mukainen tapa.

## See Also
- Elm `Regex`-dokumentaatio: [package.elm-lang.org/packages/elm/regex/latest](https://package.elm-lang.org/packages/elm/regex/latest)
- Regex101, testaa säännöllisiä lausekkeita: [regex101.com](https://regex101.com/)
- Säännöllisten lausekkeiden opas: [regular-expressions.info](https://www.regular-expressions.info/)
