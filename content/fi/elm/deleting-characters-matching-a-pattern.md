---
title:                "Merkkien poistaminen vastaavalla mallilla"
html_title:           "Arduino: Merkkien poistaminen vastaavalla mallilla"
simple_title:         "Merkkien poistaminen vastaavalla mallilla"
programming_language: "Elm"
category:             "Elm"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Mitä & Miksi?

Hahmojen poistaminen mallin mukaan on ohjelmointitehtävä, jossa tiettyjä merkkejä tai merkkijonoja poistetaan suuremmasta merkkijonosta. Ohjelmoijat tekevät tämän datan jalostamiseksi, siivoamiseksi tai järjestämiseksi.

# Kuinka tehdä

Elm-ohjelmointikielessä merkkijonoja käsitellään usein `String`-moduulin avulla. Tässä on esimerkki siitä, miten voit poistaa kaikki tietyn mallin mukaiset merkit merkkijonosta.

```Elm
import String

removeChars : String -> String -> String
removeChars pattern text =
    let
        isNotPattern char = 
            if String.fromChar char |> (\x -> String.contains pattern x) |> not then
                Just char
            else
                Nothing
    in
    String.foldr isPattern "" text
```
Kun ajat tämän koodin, esimerkiksi `removeChars "a" "banana"` palauttaa `"bnn"`.

# Syvempi sukellus

Hahmojen poistaminen mallin mukaan on konsepti, joka ylittää ohjelmointikielten. Se on ollut osa ohjelmointia lähes sen alusta lähtien, kun ohjelmoijat huomasivat tarpeen käsitellä ja siivota tekstimassaa.

Elmissä on monia muitakin tapoja hoitaa tämä. Aina ei tarvitse luoda omaa funktiota. Elm-yhteisö on kehittänyt useita paketteja, jotka tekevät tekstin käsittelystä helpompaa.

Elm takaa puhtaat toiminnot ja tyypin turvallisuuden, joten tekstinkäsittely ja merkkien poistaminen on suoraviivaista ja turvallista. Se ei kuitenkaan tue säännöllisiä lausekkeita, koska ne ovat vaikeasti ymmärrettäviä ja niitä voidaan käyttää väärin.

# Katso myös
- [Elmin virallinen String-dokumentaatio](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm-paketit tekstinkäsittelyyn](https://package.elm-lang.org/packages/elm-community/string-extra/latest/)
- [Elmin keskustelupalsta pattern matchingista](https://discourse.elm-lang.org/t/pattern-matching-strings/2329)