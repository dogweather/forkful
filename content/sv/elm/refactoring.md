---
title:                "Refaktorisering"
date:                  2024-01-26T01:17:56.862409-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refaktorisering"
programming_language: "Elm"
category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/refactoring.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Refaktorisering är i grund och botten vårstädning av din kodbas – det handlar om att omstrukturera befintlig kod utan att ändra dess externa beteende. Programmerare gör det för att göra koden mer läsbart, reducera komplexitet, förbättra underhållbarheten och göra det lättare att utöka.

## Hur man gör:
Anta att du har en Elm-funktion som gör för mycket, som att blanda användargränssnittslogik med statusuppdateringar. Den är en perfekt kandidat för refaktorisering. Ursprungligen:

```Elm
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    let
        updatedModel = { model | userInput = input }
    in
    if String.length input > 5 then
        ( updatedModel, Cmd.none )
    else
        ( model, Cmd.none )
```

Efter refaktoriseringen separerar vi bekymmer genom att dra ut logiken till olika funktioner:

```Elm
-- Uppdateringslogiken är separat
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Logiken för formatering (vy) är också separat
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Töm inmatningen om den är för kort, som ett exempelregel.

-- Uppdateringsfunktionen använder nu hjälpfunktioner
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Med dessa ändringar har du tydlig separation, och varje funktion är lättare att förstå och testa.

## Djupdykning
Refaktorisering som en formell praxis kan spåras tillbaka till programmeringens tidiga dagar när kostnaden för att ändra kod redan började ses som en kritisk aspekt av utvecklingsprocessen. Noterbart är att Martin Fowlers bok "Refaktorisering: Att förbättra utformningen av befintlig kod," publicerad i slutet av 1990-talet, verkligen satte scenen för refaktorisering med ett strukturerat angreppssätt och katalog av "kodlukter" för att identifiera refaktoriseringsmöjligheter.

I kontexten av Elm, utnyttjar refaktorisering språkets styrkor, som dess starka typsystem, vilket främjar förtroende under processen. Alternativ till manuell refaktorisering kan inkludera automatiserade kodtransformationsverktyg, men Elm:s verktyg inom detta område är fortfarande under utveckling jämfört med vissa äldre språk. Implementeringsdetaljer kretsar ofta kring vanliga refaktoriseringar som funktionsextraktion, omdöpning och förenkling av konditionaler. Elm:s kompilator är en viktig allierad i refaktoriseringen, den låter dig inte komma undan med mycket – den skriker när något är fel, och säkerställer att din refaktoriserade kod fortfarande fungerar.

## Se även
- ["Refaktorisering: Att förbättra utformningen av befintlig kod" av Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Ämnen om Refaktorisering](https://discourse.elm-lang.org/search?q=refactoring)