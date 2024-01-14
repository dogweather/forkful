---
title:                "Elm: Testien kirjoittaminen"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/writing-tests.md"
---

{{< edit_this_page >}}

## Miksi
Kirjoittaminen testien avulla on tärkeä osa ohjelmiston kehittämistä. Se auttaa varmistamaan, että koodi toimii oikein ja mahdollistaa muutosten tekemisen ilman pelkoa mahdollisista hajoamisista. Se myös helpottaa uusien kehittäjien pääsemistä nopeasti sisään projektiin ja ylläpitää koodin laatua.

## Miten
Testien kirjoittaminen Elmillä on hyvin yksinkertaista hyödyntämällä sisäänrakennettuja Unit.testing moduuleja. Alla olevassa esimerkissä testataan funktiota, joka palauttaa saman arvon kuin annettu parametri:

```Elm
import Unit

funktio x = x

Unit.test "funktio palauttaa saman parametrin" <|
    \_ -> 
        let
            tulos = funktio 5
        in
            tulos === 5 
```

Tämä testi varmistaa, että funktio palauttaa odotetun arvon, 5. Voit myös testata vääriä tulosteita varmistamalla, että makron avulla kutsuttu virhe, kuten virhetilanne, nostetaan oikein.

```Elm
import Expect exposing (equal)

funktio x = 
    if x < 0 then
        (Result.Err "Virhetilanne")
    else
        (Result.Ok x)

Expect.equal "funktio tunnistaa negatiiviset arvot" <| funktio (-5) <|
    Result.Err "Virhetilanne"
```

Tässä käytetään Expect.equal -funktiota, joka varmistaa, että virhetilanne nostetaan oikein, kun annetaan negatiivinen luku.

## Syvällinen tutkimus
Testien kirjoittaminen voi olla aikaa vievää, mutta se on tärkeä osa koodin laadun ylläpitämistä. On tärkeää kirjoittaa testit jokaiselle funktiolle ja varmistaa, että ne kattavat kaikki mahdolliset polut koodin suorituksessa. Lisäksi on hyvä myös järjestää testit niin, että ne koskevat spesifistä käyttäytymistä tai ominaisuutta, jolloin virheen paikantaminen ja korjaaminen on helpompaa.

## Katso myös
- [Elm Test -dokumentaatio](https://elm-test.readme.io/)
- [Jatkaava integrointi Elmissä](https://elm-test.readme.io/docs/continuous-integration)