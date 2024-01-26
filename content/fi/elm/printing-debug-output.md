---
title:                "Virheenjäljitystulosteiden tulostaminen"
date:                  2024-01-20T17:52:12.496761-07:00
model:                 gpt-4-1106-preview
simple_title:         "Virheenjäljitystulosteiden tulostaminen"
programming_language: "Elm"
category:             "Elm"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Mikä & Miksi?

Debug-tulostus auttaa ohjelmoijia näkemään, mitä ohjelmassa tapahtuu. Se on välttämätön, koska se valottaa ohjelman suorituksen tilaa ja auttaa virheiden jäljittämisessä.

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
