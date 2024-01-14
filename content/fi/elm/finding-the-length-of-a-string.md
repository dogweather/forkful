---
title:                "Elm: Merkkijonon pituuden löytäminen"
simple_title:         "Merkkijonon pituuden löytäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Miksi

On monia syitä miksi joku haluaisi laskea merkkijonon pituuden. Se voi olla tärkeää sovelluksissa, joissa käsitellään käyttäjän syöttämiä tietoja tai kun halutaan tarkistaa, että syöte ei ylitä tiettyä pituutta.

## Kuinka laskea merkkijonon pituus

Merkkijonon pituuden laskeminen Elm-ohjelmoinnissa on helppoa. Voit käyttää sisäänrakennettua `String.length`-funktiota, joka palauttaa merkkijonon pituuden.

```Elm
let pituus = String.length "Tervetuloa"

-- pituus:n arvoksi tulee 11
````

Voit myös laskea pituuden dynaamisesti käyttämällä muuttujia ja käyttämällä `toString`-funktiota muuttaaksesi muuttujan arvon merkkijonoksi.

```Elm
let viesti = "Hei " ++ etunimi
let pituus = String.length (toString viesti)

-- Jos etunimesi on "Matti", pituus:n arvoksi tulee 5
```

## Syvemmälle merkkijonon pituuden laskemiseen

Vaikka merkkijonon pituuden laskeminen onkin yksinkertaista, on hyödyllistä ymmärtää hieman siitä, miten se tapahtuu taustalla. Merkkijono koostuu merkeistä, joita voit ajatella numeroina. Esimerkiksi A-kirjain voi olla numero 65, B-kirjain 66 ja niin edelleen. Merkkijonon pituuden laskeminen tarkoittaa siis käytännössä merkkien lukumäärän laskemista.

## Katso myös

- [Elm String -dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-ohjelmoinnin aloittaminen -opas](https://guide.elm-lang.org)