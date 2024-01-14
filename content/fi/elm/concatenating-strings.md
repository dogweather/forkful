---
title:                "Elm: Merkkijonojen yhdistäminen"
simple_title:         "Merkkijonojen yhdistäminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/concatenating-strings.md"
---

{{< edit_this_page >}}

## Miksi

Miksi kannattaa yhdistää merkkijonoja ohjelmoinnissa?

## Kuinka tehdä

Voimme yhdistää merkkijonoja kahdella eri tavalla Elm-kielen avulla. Voimme käyttää `++`-operaattoria tai `String.concat`-funktiota. Katso esimerkit alla olevista koodiblokeista. 

```Elm
main =
  let
      string1 = "Hei"
      string2 = "maailma"
      combinedString = string1 ++ " " ++ string2
   in
      combinedString

```

Tulostus: 

> "Hei maailma"

```Elm
main =
  let
      strings = ["Hei", "kaverit!"]
      combinedString = String.concat strings
   in
      combinedString
```

Tulostus: 

> "Hei kaverit!"

## Syvällinen sukellus

Concatenoiminen on hyödyllinen työkalu, kun haluamme yhdistää kaksi tai useampia merkkijonoja yhdeksi kokonaisuudeksi. Tämä voi olla hyödyllistä esimerkiksi silloin, kun luomme dynaamisia viestejä tai käsittelemme käyttäjän syöttämiä tietoja. Lisäksi concatenoiminen voi auttaa meitä tekemään koodiesteettisesti miellyttävämpää ja helposti luettavaa. 

Muista myös, että `++`-operaattori toimii vain kahdelle merkkijonolle, kun taas `String.concat`-funktiolla voimme yhdistää useampia merkkijonoja kerralla. 

## Katso myös

- [Elm-kielen dokumentaatio merkkijonoista](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Miten yhdistää listoja Elm-kseleen](https://elmprogramming.com/concatenating-lists-elm-example.html)
- [Elm-tutoriaali aloittelijoille](https://www.tutorialspoint.com/elm/index.htm)