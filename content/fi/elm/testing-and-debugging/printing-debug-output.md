---
date: 2024-01-20 17:52:12.496761-07:00
description: "Mik\xE4 & Miksi? Debug-tulostus auttaa ohjelmoijia n\xE4kem\xE4\xE4\
  n, mit\xE4 ohjelmassa tapahtuu. Se on v\xE4ltt\xE4m\xE4t\xF6n, koska se valottaa\
  \ ohjelman suorituksen tilaa ja\u2026"
lastmod: 2024-02-19 22:05:15.385195
model: gpt-4-1106-preview
summary: "Mik\xE4 & Miksi? Debug-tulostus auttaa ohjelmoijia n\xE4kem\xE4\xE4n, mit\xE4\
  \ ohjelmassa tapahtuu. Se on v\xE4ltt\xE4m\xE4t\xF6n, koska se valottaa ohjelman\
  \ suorituksen tilaa ja\u2026"
title: "Virheenj\xE4ljitystulosteiden tulostaminen"
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
