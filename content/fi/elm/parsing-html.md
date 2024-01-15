---
title:                "HTML-analyysi"
html_title:           "Elm: HTML-analyysi"
simple_title:         "HTML-analyysi"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi

Vanhat HTML-virheet, kuten puuttuvat sulkumerkit, voivat olla katastrofaalisia verkkosivuston toiminnalle. Parsimisen avulla voit tarkistaa koodin ja varmistaa sen oikeellisuuden, mikä on tärkeää sivuston käyttökokemuksen kannalta.

## Näin

```Elm
import Html
import Html.Parser exposing (..)

htmlString : String
htmlString =
    "<p>Tervetuloa</p>"

parsedHtml : Result (List Html.Error) Html.Html
parsedHtml =
    parse htmlString

```

Tässä koodiesimerkissä käytämme Html.Parser kirjastoa, joka sisältää funktion parse, joka ottaa merkkijonon argumenttina ja palauttaa joko Html.Html:n tai virheen. Voit myös käyttää muita Html.Parser kirjaston funktioita, kuten parseFragment, joka palauttaa fragmentin.

Tarkastellaan seuraavaksi tulostetta. Jos HTML on oikein, toimii seuraava tulostus:

```Elm
Ok
    [ { name = "p", attributes = [], children = [ Text "Tervetuloa" ] } ]
```

Jos HTML sisältää virheitä, se palauttaa virheilmoituksen, joka auttaa sinua paikantamaan ja korjaamaan ongelmat.

## Deep Dive

Elm kieli käyttää HTML:n DOM puuta, joten HTML:n parsiminen ei ole kielen kotitehtävä. Sen sijaan Elm käyttää parser-kirjastoja, kuten Html.Parser, joka on kehitetty nimenomaan parsimista varten.

HTML:n parsiminen mahdollistaa myös XML:n parsimisen. Elm Parser on rakennettu käyttäen jotain nimeltä “parser combinator”, mikä on modulaarinen tapa luoda parser-sääntöjä. Parser combinatorit ovat sarja funktionaalisia työkaluja, joita voit käyttää luomaan uusia parsereita monimutkaisten HTML tasojen käsittelyyn.

## Katso myös
[Virallinen Elm Parser dokumentaatio](https://package.elm-lang.org/packages/elm/parser/latest/)

[Tutorial video HTML parsimisesta Elm:ssä](https://www.youtube.com/watch?v=-zJz4S96xN8)

[Practical Elm -blogi HTML parsimisesta](https://dev.to/rtfeldman/parsing-html-in-elm)