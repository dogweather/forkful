---
title:                "Elm: Vianetsimistulostus"
simple_title:         "Vianetsimistulostus"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Miksi
Debuggaustulosteen tulostaminen on tärkeä työkalu koodaajalle, joka auttaa hahmottamaan ohjelman toimintaa. Se auttaa tunnistamaan virheitä ja löytämään ongelmakohtia koodista.

## Miten
Debuggaustulosteen tulostaminen on helppoa Elm-kielellä. Voit käyttää `Debug.log` -funktiota ja tulostaa haluamasi tiedot konsoliin. Alla on esimerkki koodista, joka tulostaa taulukon numeroiden neliöjuuret konsoliin käyttäen `Debug.log` -funktiota.

```Elm
import Debug

numbers = [1, 4, 9, 16, 25]
squaredRoots = List.map (\x -> Debug.log (toString (x)) (Math.sqrt (toFloat x))) numbers

-- Konsoliin tulostetaan seuraavaa:
-- 1
-- 4
-- 9
-- 16
-- 25
```

## Syvä Sukellus
Printtaaminen debug-tietoja voi auttaa sinua ymmärtämään ohjelmasi suoritusta paremmin. Voit tulostaa monimutkaisempia tietorakenteita, kuten lista ja sanakirjoja, koodiesimerkin tapaan. Tämä auttaa kehittäjää hahmottamaan datan rakennetta ja havaitsemaan mahdollisia virheitä.

On tärkeää huomata, että debuggaustulosteen tulostamisen tulee tapahtua vain kehityskauden aikana. Lopulliseen tuotantoversioon tulostamiset tulisi poistaa, jotta koodi ei näytä ylimääräistä tietoa loppukäyttäjälle.

## Katso myös
- [Elm dokumentointi](https://guide.elm-lang.org/)
- [Debuggaus Elm: n avulla](https://www.elm-tutorial.org/en/03-subs-cmds/02-debugging.html)