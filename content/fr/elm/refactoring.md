---
title:                "Refactoring"
date:                  2024-01-26T01:17:55.425347-07:00
model:                 gpt-4-0125-preview
simple_title:         "Refactoring"

category:             "Elm"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/refactoring.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Le remaniement (ou refactoring) est essentiellement le grand ménage de votre base de code - il s'agit de restructurer le code existant sans en changer le comportement externe. Les programmeurs le font pour rendre le code plus lisible, réduire la complexité, améliorer la maintenabilité, et faciliter son extension.

## Comment faire :
Imaginez que vous avez une fonction Elm qui fait trop de choses, comme mélanger la logique de l'interface utilisateur avec les mises à jour d'état. C'est un candidat parfait pour le refactoring. À l'origine :

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

Après refactoring, nous séparons les préoccupations en extrayant la logique dans différentes fonctions :

```Elm
-- La logique de mise à jour est séparée
updateUserInput : String -> Model -> Model
updateUserInput input model = 
    { model | userInput = input }

-- La logique de formatage (vue) est également séparée
formatUserInput : Model -> (Model, Cmd Msg)
formatUserInput model =
    if String.length model.userInput > 5 then
        ( model, Cmd.none )
    else
        ( { model | userInput = "" }, Cmd.none ) -- Effacer l'entrée si elle est trop courte, comme règle d'exemple.

-- La fonction de mise à jour utilise maintenant des fonctions d'aide
updateAndFormat : String -> Model -> (Model, Cmd Msg)
updateAndFormat input model =
    model
    |> updateUserInput input
    |> formatUserInput
```
Avec ces changements, vous avez une séparation claire et chaque fonction est plus facile à comprendre et à tester.

## Plongée profonde
Le refactoring en tant que pratique formelle remonte aux premiers jours de la programmation, lorsque le coût de modification du code était déjà reconnu comme un aspect critique du processus de développement. Notamment, le livre de Martin Fowler "Refactoring: Improving the Design of Existing Code", publié à la fin des années 1990, a vraiment préparé le terrain pour le refactoring avec une approche structurée et un catalogue de "code smells" pour identifier les opportunités de refactoring.

Dans le contexte d'Elm, le refactoring tire parti des forces du langage, comme son système de typage fort, qui favorise la confiance pendant le processus. Les alternatives au refactoring manuel peuvent inclure des outils de transformation de code automatisés, mais l'outillage d'Elm dans ce domaine est encore en maturation par rapport à certains langages plus anciens. Les détails d'implémentation tournent souvent autour des refactorings communs comme l'extraction de fonctions, le renommage et la simplification des conditionnels. Le compilateur Elm est un allié clé dans le refactoring, car il ne vous laisse pas faire beaucoup d'erreurs - il crie chaque fois que quelque chose ne va pas, assurant que votre code refactorisé fonctionne toujours.

## Voir aussi
- ["Refactoring: Improving the Design of Existing Code" par Martin Fowler](https://martinfowler.com/books/refactoring.html)
- [Elm Discourse - Sujets sur le Refactoring](https://discourse.elm-lang.org/search?q=refactoring)
