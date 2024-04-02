---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:00:46.892857-07:00
description: "De huidige datum in Elm krijgen betekent het ophalen van de huidige\
  \ kalenderdatum van het systeem. We doen dit om gebeurtenissen te timestampen, taken\
  \ te\u2026"
lastmod: '2024-03-13T22:44:50.735541-06:00'
model: gpt-4-0125-preview
summary: "De huidige datum in Elm krijgen betekent het ophalen van de huidige kalenderdatum\
  \ van het systeem. We doen dit om gebeurtenissen te timestampen, taken te\u2026"
title: Het huidige datum ophalen
weight: 29
---

## Wat & Waarom?
De huidige datum in Elm krijgen betekent het ophalen van de huidige kalenderdatum van het systeem. We doen dit om gebeurtenissen te timestampen, taken te plannen of duurtijden bij te houden.

## Hoe:
Elm behandelt datums met de `Time` module. Je krijgt de huidige tijd als een POSIX-tijdstempel, die je dan omzet naar een datum.

```Elm
import Browser
import Task
import Time

type Msg = GetCurrentTime Time.Posix

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    geval msg van
        GetCurrentTime posixTime ->
            laat
                -- Zet POSIX tijd om naar een datadatum record
                datum = Time.toDate posixTime
            in
            -- Update je model dienovereenkomstig hier
            ({ model | date = datum }, Cmd.none)

-- Om het verkrijgen van de huidige tijd te initiëren
getCurrentTime : Cmd Msg
getCurrentTime =
    Task.perform GetCurrentTime Time.now

-- Voorbeeld output:
-- datum { jaar = 2023, maand = Mrt, dag = 26 }
```

## Diepgaande duik
In oudere webtalen is het verkrijgen van de datum een kwestie van een regel code. Elm is anders. Het maakt neveneffecten zoals het verkrijgen van de huidige tijd expliciet door de Elm Architectuur. Dit moedigt zuiverheid en onderhoudbaarheid van de code aan.

Alternatieven zijn het gebruik van pakketten van derden of het afhandelen van datums in je servercode en deze doorgeven aan Elm via flags of poorten.

Wat implementatie betreft, krijgt Elm's `Time.now` de tijd als een POSIX-tijdstempel (milliseconden sinds het Unix-tijdperk). Dit is onafhankelijk van tijdzones, en je kunt het naar wens formatteren met behulp van functies uit de `Time` module.

## Zie ook
- [Elm Time documentatie](https://package.elm-lang.org/packages/elm/time/latest/)
- [Elms gids voor commando's en abonnementen](https://guide.elm-lang.org/effects/)
