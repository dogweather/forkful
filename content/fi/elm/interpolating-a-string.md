---
title:                "Merkkijonon interpolointi"
html_title:           "Bash: Merkkijonon interpolointi"
simple_title:         "Merkkijonon interpolointi"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Mikä ja miksi?
Stringin interpolaatio on tapa liittää muuttujien arvoja suoraan merkkijonojen sisälle. Koodaajat turvautuvat tähän yksinkertaistamaan ja selkiyttämään koodia.

## Miten:
Elm-kieli ei tue suoraan stringin interpolaatiota, mutta sen voi saavuttaa liitännäisfunktioiden, kuten `++`(concatenation) kautta. Tässä on esimerkki:

```Elm
tervehdys : String -> String
tervehdys nimi =
    "Hei, " ++ nimi ++ "! Kuinka voit?"

main = 
    print (tervehdys "Pekka")
```
Kun tämä koodi ajetaan, se tulostaa: `Hei, Pekka! Kuinka voit?`

## Syvempi sukellus:
Vaikka stringin interpolaatio on ollut mukana useimmissa ohjelmointikielissä aina alkuajoista lähtien, Elm-päättäjät ovat päättäneet olla toteuttamatta sitä kielen yksinkertaisuuden takia. Sen sijaan Elm tarjoaa '++' operaattorin, joka liittää yhteen kaksi stringiä.

Elm:n lisäksi jotkut muut kielet, kuten Python ja JavaScript, tukevat stringin interpolaatiota käyttäen erityistä syntaksia, esim. `'Hei ${nimi}! Kuinka voit?'`.

## Katso myös:
- Strings in Elm: [https://elmprogramming.com/strings.html]
- Elm Concatenation: [https://elm-lang.org/docs/syntax#operators]