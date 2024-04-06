---
date: 2024-01-20 17:52:12.496761-07:00
description: "How to: Miten: Elmiss\xE4 debug-tulostuksen saa esiin `Debug.log`-funktiolla.\
  \ Se tulostaa arvon ja palauttaa sen sellaisenaan, joten voit sijoittaa sen miss\xE4\
  \u2026"
lastmod: '2024-04-05T22:38:57.094882-06:00'
model: gpt-4-1106-preview
summary: "Miten: Elmiss\xE4 debug-tulostuksen saa esiin `Debug.log`-funktiolla. Se\
  \ tulostaa arvon ja palauttaa sen sellaisenaan, joten voit sijoittaa sen miss\xE4\
  \ tahansa koodissasi."
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
weight: 33
---

## How to:
Miten:

Elmissä debug-tulostuksen saa esiin `Debug.log`-funktiolla. Se tulostaa arvon ja palauttaa sen sellaisenaan, joten voit sijoittaa sen missä tahansa koodissasi.

```Elm
import Html
import Debug

main =
  Html.text (Debug.log "DebugMessage" "Hello, Elm!")

```

Tämä koodi tulostaa konsoliin "DebugMessage: "Hello, Elm!"" ja näyttää viestin "Hello, Elm!" selaimessa.

## Deep Dive
Syväsukellus:

Elm otti `Debug.log`-funktion käyttöön varhaisessa vaiheessa, jotta ohjelmoijille tarjottiin yksinkertainen tapa tarkkailla ohjelmansa tilaa. Historiallisesti monet kielet, kuten JavaScript, ovat käyttäneet `console.log`-funktiota samassa tarkoituksessa. Ellei debug-tulostusta tarvita, Elm kannustaa käyttämään puhdasta funktiota ilman sivuvaikutuksia. `Debug.log` on työkalu kehityksen aikana, ei tuotantokoodissa. Käyttö voi myös aiheuttaa suorituskykyongelmia, jos tulostettavaa dataa on paljon.

## See Also
Katso Myös:

Elm Debugger: https://guide.elm-lang.org/debugging/
Elm Debug.log dokumentaatio: https://package.elm-lang.org/packages/elm/core/latest/Debug#log
Elm-yhteisön keskusteluja: https://discourse.elm-lang.org/
