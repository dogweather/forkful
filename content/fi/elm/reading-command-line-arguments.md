---
title:                "Elm: Komentoriviparametrien lukeminen"
simple_title:         "Komentoriviparametrien lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Miksi
Command line -argumenttien lukeminen voi olla hyödyllistä sovelluskehittäjille, jotka haluavat antaa käyttäjilleen mahdollisuuden ohjailla sovelluksiaan erilaisten parametrien avulla.

## Miten
Käytettäessä Elm-ohjelmointikieltä, voit käyttää `CMD` -moduulia lukemaan ja käsittelemään komentoriviargumentteja. Seuraavassa esimerkissä näytämme, kuinka voit lukea ja tulostaa yhden argumentin:

```Elm
import String
import CMD exposing (args)

main = 
    args
    |> List.head
    |> Maybe.withDefault "Ei argumenttia saatavilla."
    |> String.reverse
    |> String.toUpper
    |> Debug.log "Tulos: "
```

Alla on esimerkki, joka ottaa vastaan kaksi argumenttia ja kerrotaan ne keskenään:

```Elm
import Time
import CMD exposing (args)

multiplyArgs arg1 arg2 = 
    let 
        in1 = String.toInt arg1
        in2 = String.toInt arg2
    in
        case (in1, in2) of
            (Just i1, Just i2) -> 
                i1 * i2
                |> toString
                |> Debug.log "Tulos: "
    
            _ -> 
                Debug.crash "Argumentit eivät ole numeroita."

main = 
    args
    |> List.head
    |> Maybe.withDefault "Ei argumenttia saatavilla."
    |> Debug.log "Ensimmäinen argumentti: "
    |> args
    |> List.tail
    |> Maybe.withDefault []
    |> List.head
    |> Maybe.withDefault "Ei toista argumenttia annettu."
    |> Debug.log "Toinen argumentti: "
    |> multiplyArgs
```

Suoritettuna komentoriviltä käsin esimerkin kanssa, joka ottaa yhden argumentin, tulostus voisi olla seuraavanlainen:

```
Ei argumenttia saatavilla.
Tulos: EIKODIM YNOMI
```

Ja esimerkki, joka ottaa kaksi argumenttia, voisi tuottaa seuraavan tulostuksen:

```
Ensimmäinen argumentti: 5
Toinen argumentti: 8
Tulos: 40
```

## Syvempään
CMD-moduuli tarjoaa myös muita hyödyllisiä funktioita, kuten `run` ja `path`, jotka voivat auttaa käsittelemään komentoriviargumentteja. Voit tutustua niihin Elm-oppaassa ja lisätietoja löytyy myös CMD-moduulin dokumentaatiosta.

## Katso myös
- [Elm-opas](https://guide.elm-lang.org/)
- [CMD-moduulin dokumentaatio](https://package.elm-lang.org/packages/elm/cmd/latest/)