---
title:                "Komentorivin argumenttien lukeminen"
html_title:           "Elm: Komentorivin argumenttien lukeminen"
simple_title:         "Komentorivin argumenttien lukeminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Mikä & Miksi?

Komennonriviparametrien lukeminen tarkoittaa käyttäjän syöttämien komentojen tulkitsemista ja niiden käyttämistä ohjelman ohjaamiseen. Tämä on hyödyllistä, koska se mahdollistaa joustavuuden ohjelman käyttäytymisessä käyttäjän syötteiden perusteella.

## Miten se tehdään:

Seuraavassa koodiesimerkissä näytetään, miten lukea ja käsitellä komennonriviparametreja Elm-ohjelmointikielessä.

```Elm
import Process

main =
    Process.commandLineArguments
    |> List.map String.toUpper
    |> List.filter (\arg -> String.length arg > 1)
    |> List.sort
    |> String.join " "
    |> Console.log
```

Esimerkiksi aja tämä koodi komennolla `elm make main.elm -- moikka maailma`, ja se tulostaa: `MAAILMA MOIKKA`.

## Syvempi katsaus:

(1) Historiallisessa kontekstissa komennonriviparametreja on käytetty laajasti jo ennen graafisten käyttöliittymien aikakautta. Ne tarjoavat tavan antaa komentoja ohjelmalle ja kontrolloida sen toimintaa. 

(2) On olemassa muitakin tapoja syöttää tietoa ohjelmaan, kuten lukea tiedostoja, käyttää verkko-rajapintoja tai kysyä käyttäjältä syötteitä dynaamisesti käyttöliittymän kautta. 

(3) Elm-ohjelmassa, `Process.commandLineArguments` palauttaa listan komennonriviparametreista. Listaa voidaan käsitellä monin tavoin sen mukaan, mitä tarvitset ohjelmasi tekemään.

## Katso myös:

Elm-kirjastojen dokumentaation voi löytää seuraavista lähteistä, joissa on lisätietoa aiheesta.

- [Elm Process](https://package.elm-lang.org/packages/elm/core/latest/Platform-Cmd#Cmd)
- [Elm String](https://package.elm-lang.org/packages/elm/core/latest/String)
- [Elm List](https://package.elm-lang.org/packages/elm/core/latest/List)