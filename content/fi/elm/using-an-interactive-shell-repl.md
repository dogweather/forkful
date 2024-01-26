---
title:                "Interaktiivisen komentotulkin (REPL) käyttö"
date:                  2024-01-26T04:13:24.701823-07:00
model:                 gpt-4-0125-preview
simple_title:         "Interaktiivisen komentotulkin (REPL) käyttö"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Mitä & Miksi?
Read-Eval-Print Loop (REPL) on yksinkertainen, interaktiivinen ohjelmointiympäristö, joka ottaa vastaan yksittäisiä käyttäjän syötteitä, arvioi ne ja palauttaa tuloksen käyttäjälle. Elm-ohjelmoijat käyttävät REPL:ää nopeisiin kokeiluihin, vianetsintään tai kielen oppimiseen.

## Kuinka:
Elm ei sisällä integroitua REPL:iä. Voit kuitenkin käyttää `elm repl` komentoa komentorivillä käynnistääksesi Elmin istunnon Elm:n asentamisen jälkeen.

```Elm
> import List exposing (..)
> map (\x -> x * 2) [1, 2, 3, 4]
[2,4,6,8] : List number
```

Tässä istunnossa, List-funktiot tuotua, kaksinkertaistimme lukulistassa olevat numerot ja saimme tuloksen välittömästi.

## Syväsukellus
Elmin REPL voi tuntua rajoitetulta verrattuna joidenkin muiden kielten, kuten Pythonin tai JavaScriptin, REPL:iin, koska Elm on kääntävä kieli, joka keskittyy web-sovellusten tuottamiseen. Historiallisesti Elm on keskittynyt kokonaisiin sovelluksiin pikemminkin kuin skriptaukseen tai kuorivuorovaikutukseen.

Vaihtoehtoja Elmin REPL:lle sisältävät `elm-live` ja verkon muokkausympäristöt kuten Ellie, missä voit nähdä koodimuutokset reaaliajassa selaimessa.

Toteutukseen liittyen, Elm REPL kääntää Elmin koodinpätkiä JavaScriptiksi taustalla, mikä sallii Elmin interaktiivisen käytön. Tämä eroaa tulkittavien kielien REPL:stä, jotka eivät tarvitse tätä käännösvaihetta. Elm REPL on myös karsittu pitämään ydinkieli kevyenä ja keskittyneenä.

## Katso myös
- Elmin virallinen opas vuorovaikutteisuudesta: https://guide.elm-lang.org/interop/
- Ellie, verkkopohjainen Elm-leikkikenttä: https://ellie-app.com/new
- `elm-live`, joustava kehityspalvelin Elmille: https://www.elm-live.com/