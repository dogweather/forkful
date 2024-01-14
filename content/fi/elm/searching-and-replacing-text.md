---
title:    "Elm: Tekstin etsiminen ja korvaaminen"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Miksi

Miksi jonkun kannattaa käyttää hakemisto- ja korvaustoimintoja tekstissä? Hakemisto- ja korvaustoiminnon avulla voit nopeuttaa koodaamista ja vähentää virhetilanteita tekemällä massiivisia muutoksia tekstiin kerralla.

## Kuinka tehdä

Yksi tapa käyttää hakemisto- ja korvaustoimintoja on käyttämällä Elm-ohjelmointikieltä. Se on puhdas ja turvallinen ohjelmointikieli, joka auttaa sinua tekemään muutoksia turvallisesti ja tehokkaasti. Alla on esimerkki koodista ja sen tuottamasta tulosteesta.

```Elm
import SearchAndReplace exposing (replace)

replace "Hei!" "Moi!" "Hei maailma!"
```
Tulostaa: `Moi maailma!`

## Syventyvä tieto

Hakemisto- ja korvaustoimintojen avulla voit tehdä monimutkaisempia muutoksia teksteihin käyttämällä säännöllisiä lausekkeita. Voit esimerkiksi korvata kaikki numerot kirjaimilla käyttämällä `\d` ja `\w` säännöllisiä lausekkeita.

```Elm
import Regex exposing (replace, allMatches)

replace (Regex.regex "\\d+") (\_ -> "---") "Hei 123 maailma!"
```
Tulostaa: `Hei --- maailma!`

## Katso myös

- [Elm ohjelmointikieli](https://elm-lang.org/)
- [Säännölliset lausekkeet](https://www.regular-expressions.info/)
- [Hakemisto- ja korvaustoimintojen käyttö Elm-paketteihin](https://package.elm-lang.org/)