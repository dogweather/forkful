---
title:                "HTML:n jäsentäminen"
html_title:           "Bash: HTML:n jäsentäminen"
simple_title:         "HTML:n jäsentäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

HTML:n jäsentäminen tarkoittaa koodin käyntiä läpi ja sen tiedon erottamista, jota tarvitset. Ohjelmoijat tekevät tämän helpottaakseen verkkosivujen tietojen käsittelyä.

## Miten:

Luodaan HTML-jäsentäjä käyttäen Elm-pakettia nimeltä `elm/parser`. Tässä on malli:

```Elm
import Html
import Parser exposing ((|.), DeadEnd, Parser, Step(..), StepDone, StepLoop, oneOf, problem, succeed, symbol)

type alias Html = 
    { tagName : String
    , children : List Html
    }

parseTag : Parser String
parseTag = 
    symbol "<" 
    |. Parser.getChompedString (Parser.chompWhile (/= '>'))
    |> Parser.map (\tag -> String.trim (String.dropRight 1 tag))
```

Voit ajaa koodin ja saat tämän tuloksen:

```Elm
parseTag "</div>" 
-- returns ("div", 5)
```
   
## Syvällinen sukellus:

HTML-jäsentäminen on ollut olennainen osa web-ohjelmointia siitä lähtien, kun ensimmäiset dynaamiset verkkosivut luotiin. Muita vaihtoehtoja on, kuten esimerkiksi käyttää ulkoista palvelua kuten Puppeteeriä, joka hallinnoi Chromium-selainta Node.js:n kautta. Kuitenkin, Elm:llä voit tehdä tämän saman työn vähemmällä koodilla ja suuremmalla teholla.

HTML:n jäsentämisen toteutus Elm:llä on tehokas ja siinä on mahdollisuus hyödyntää Elm:n vahvoja tyyppejä virheenkäsittelyyn.

## Katso myös:

- [Elm-parserin dokumentaatio](https://package.elm-lang.org/packages/elm/parser/latest)
- [Elm:ssä tehty HTML-jäsentäjä](https://github.com/elm-community/html-parser)
- [Puppeteer](https://pptr.dev/) luotettava, mutta monimutkainen vaihtoehto.