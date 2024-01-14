---
title:                "Elm: Html:n jäsentäminen"
simple_title:         "Html:n jäsentäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/parsing-html.md"
---

{{< edit_this_page >}}

## Miksi: Miksi osallistua HTML:n jäsentämiseen?

HTML:n jäsentäminen on tärkeä taito, joka mahdollistaa verkkosivujen rakenteen ja sisällön käsittelyn ohjelmallisesti. Se säästää aikaa ja vaivaa, kun haluamme muokata tai hakea tietoa verkkosivuilta.

## Kuinka: Esimerkkejä koodikatkelmilla ja tulosteilla

```
Elm.format "<div><p>Hei maailma!</p></div>" 
```
```
Tuloste: 
Div [][P [] [Text "Hei maailma!"]]
```

Tässä yksinkertaisessa esimerkissä elm-format -funktio jäsentää HTML:n ja palauttaa sen elm-syntaxina. Tämä syntaxi voidaan sitten käsitellä ohjelmallisesti halutulla tavalla.

## Syvemmälle: Tietoa HTML:n jäsentämisestä

HTML:n jäsentäminen voi olla monimutkaista, sillä se sisältää paljon erilaisia elementtejä ja atribuutteja. Elm tarjoaa kuitenkin hyödyllisiä työkaluja, kuten elm-html -kirjaston, joka helpottaa HTML:n käsittelyä.

Tässä on esimerkki, joka käyttää HTML:n lukemista ja jäsentämistä elm-html kirjastolla:

``` 
-- HTML:n lukeminen
mukautettuTagi = HTML.attribute "custom-tag" "elmi" [HTML.text "Tämä on tekstiä"]

-- HTML:n jäsentäminen
elmHtmlTagi = mukautettuTagi [] 

```

Tutustu Elm:n opas-sivustoon löytääksesi lisätietoa HTML:n jäsentämisestä ja elm-html -kirjastosta.

## Katso myös

- [Oficial Elm -sivusto](https://guide.elm-lang.org/)
- [elm-html -kirjaston dokumentaatio](https://package.elm-lang.org/packages/elm/html/latest/)
- [Elm -ohjelmointiyhteisö Suomessa](https://www.quora.com/Where-can-I-find-the-Scandinavian-programming-community)