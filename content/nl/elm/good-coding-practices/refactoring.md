---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:05:54.207773-07:00
description: "Refactoring is in wezen het grote schoonmaak houden van je codebasis\
  \ - het gaat over het herstructureren van bestaande code zonder het externe gedrag\u2026"
lastmod: 2024-02-19 22:05:09.787342
model: gpt-4-0125-preview
summary: "Refactoring is in wezen het grote schoonmaak houden van je codebasis - het\
  \ gaat over het herstructureren van bestaande code zonder het externe gedrag\u2026"
title: Refactoring
---

{{< edit_this_page >}}

## Wat & Waarom?
Refactoring is in wezen het grote schoonmaak houden van je codebasis - het gaat over het herstructureren van bestaande code zonder het externe gedrag ervan te veranderen. Programmeurs doen dit om de code leesbaarder te maken, complexiteit te verminderen, onderhoudbaarheid te verbeteren en het gemakkelijker te maken om uit te breiden.

## Hoe:
Stel je hebt een Elm-functie die te veel doet, zoals het mixen van UI-logica met statusupdates. Het is een perfecte kandidaat voor refactoring. Oorspronkelijk:

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

Na refactoring scheiden we problemen door de logica in verschillende functies uit te splitsen:

```Elm
-- Update logica is gescheiden
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- De formatterings (weergave) logica is ook gescheiden
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Wis invoer als deze te kort is, als een voorbeeldregel.

-- Updatefunctie gebruikt nu hulpfuncties
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Met deze wijzigingen heb je een duidelijke scheiding, en elke functie is gemakkelijker te begrijpen en te testen.

## Diepere Duik
Refactoring als een formele praktijk kan worden teruggevoerd tot de vroege dagen van programmeren toen de kosten van het wijzigen van code al werd erkend als een kritisch aspect van het ontwikkelingsproces. Opvallend is dat het boek van Martin Fowler "Refactoring: Improving the Design of Existing Code", gepubliceerd in de late jaren '90, echt het toneel zette voor refactoring met een gestructureerde aanpak en catalogus van "codegeuren" om refactoringmogelijkheden te identificeren.

In de context van Elm maakt refactoring gebruik van de sterke punten van de taal, zoals het sterke type systeem, wat vertrouwen bevordert tijdens het proces. Alternatieven voor handmatige refactoring kunnen geautomatiseerde code-transformatietools omvatten, maar Elm's gereedschappen op dit gebied zijn nog aan het rijpen in vergelijking met sommige oudere talen. Implementatiedetails draaien vaak om gangbare refactorings zoals het extraheren van functies, hernoemen en het vereenvoudigen van conditionals. Elm's compiler is een belangrijke bondgenoot bij het refactoren, omdat het je niet veel laat wegkomen - het schreeuwt wanneer er iets mis is, zodat je gerefactorde code nog steeds werkt.

## Zie Ook
- ["Refactoring: Improving the Design of Existing Code" door Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Onderwerpen over Refactoring](https://discourse.elm-lang.org/search?q=refactoring)
