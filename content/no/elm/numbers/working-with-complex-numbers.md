---
date: 2024-01-26 04:39:23.662224-07:00
description: "Hvordan: Elm har ikke innebygd st\xF8tte for komplekse tall, s\xE5 du\
  \ m\xE5 opprette din egen type og funksjoner. Her er en rask oppsett."
lastmod: '2024-03-13T22:44:40.702420-06:00'
model: gpt-4-0125-preview
summary: "Elm har ikke innebygd st\xF8tte for komplekse tall, s\xE5 du m\xE5 opprette\
  \ din egen type og funksjoner."
title: "\xC5 jobbe med komplekse tall"
weight: 14
---

## Hvordan:
Elm har ikke innebygd støtte for komplekse tall, så du må opprette din egen type og funksjoner. Her er en rask oppsett:

```Elm
type alias Complex =
    { real : Float, imaginary : Float }

add : Complex -> Complex -> Complex
add a b =
    { real = a.real + b.real, imaginary = a.imaginary + b.imaginary }

-- Eksempel på bruk:
a = { real = 3, imaginary = 2 }
b = { real = 1, imaginary = -4 }

sum = add a b
-- sum er { real = 4.0, imaginary = -2.0 }
```

## Dypdykk
Historisk sett ble ikke komplekse tall alltid akseptert. De ble en spillveksler på 1500-tallet for å løse kubiske ligninger. Alternativer i andre språk som Python tilbyr innebygd støtte for komplekse tall med operasjoner rett fra boksen. Elm krever en Gjør-Det-Selv tilnærming som du har sett. Men du kan gjøre det så sofistikert som nødvendig, bygge multiplikasjon, divisjon, og andre operasjoner, og justere ytelsesproblemer.

## Se Også
- Elms offisielle dokumentasjon: https://package.elm-lang.org/ for å skape egendefinerte typer og mestre Elm grunnleggende.
- Matematikkhistorieentusiaster kunne sjekke ut "An Imaginary Tale" av Paul J. Nahin for en reise gjennom de komplekse tallenes tid.
- Dykk inn i matteorienterte programmeringsutfordringer på Project Euler (https://projecteuler.net) for å anvende din kunnskap om komplekse tall.
