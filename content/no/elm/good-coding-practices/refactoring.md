---
date: 2024-01-26 01:17:49.474716-07:00
description: "Refaktorering er i bunn og grunn \xE5 gj\xF8re en v\xE5rrengj\xF8ring\
  \ i koden din \u2013 det handler om \xE5 restrukturere eksisterende kode uten \xE5\
  \ endre dens eksterne\u2026"
lastmod: '2024-03-13T22:44:40.717850-06:00'
model: gpt-4-0125-preview
summary: "Refaktorering er i bunn og grunn \xE5 gj\xF8re en v\xE5rrengj\xF8ring i\
  \ koden din \u2013 det handler om \xE5 restrukturere eksisterende kode uten \xE5\
  \ endre dens eksterne\u2026"
title: Refaktorering
weight: 19
---

## Hva & Hvorfor?
Refaktorering er i bunn og grunn å gjøre en vårrengjøring i koden din – det handler om å restrukturere eksisterende kode uten å endre dens eksterne oppførsel. Programmerere gjør det for å gjøre koden mer lesbar, redusere kompleksitet, forbedre vedlikeholdbarheten og gjøre det lettere å utvide.

## Hvordan:
Anta at du har en Elm-funksjon som gjør for mye, som å blande UI-logikk med oppdateringer av tilstand. Det er en perfekt kandidat for refaktorering. Opprinnelig:

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

Etter refaktoreringen, separerer vi bekymringene ved å trekke ut logikken i forskjellige funksjoner:

```Elm
-- Oppdateringslogikken er separat
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- Formaterings- (visnings-) logikken er også separat
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Tøm inndata hvis den er for kort, som et eksempelregel.

-- Oppdateringsfunksjonen bruker nå hjelpefunksjoner
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Med disse endringene har du tydelig separasjon, og hver funksjon er lettere å forstå og teste.

## Dypdykk
Refaktorering som en formell praksis kan spores tilbake til programmeringens tidlige dager, da kostnaden ved å endre kode allerede ble anerkjent som en kritisk aspekt av utviklingsprosessen. Spesielt satte Martin Fowlers bok "Refaktorering: Forbedring av designet på eksisterende kode," publisert på slutten av 1990-tallet, scenen for refaktorering med en strukturert tilnærming og katalog av "kode lukter" for å identifisere refaktoreringsmuligheter.

I konteksten av Elm, utnytter refaktorering språkets styrker, som dets sterke typesystem, som fremmer tillit under prosessen. Alternativer til manuell refaktorering kan inkludere automatiserte kode-transformasjonsverktøy, men Elms verktøy på dette området er fortsatt under modning sammenlignet med noen eldre språk. Implementeringsdetaljer kretser ofte rundt vanlige refaktoreringer som funksjonsekstraksjon, omdøping, og forenkling av betingelser. Elms kompilator er en nøkkelalliert i refaktorering, da den ikke lar deg slippe unna med mye – den skriker hver gang noe er galt, og sikrer at din refaktorerte kode fortsatt fungerer.

## Se også
- ["Refaktorering: Forbedring av designet på eksisterende kode" av Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Diskurs - Emner om Refaktorering](https://discourse.elm-lang.org/search?q=refactoring)
