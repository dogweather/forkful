---
date: 2024-01-20 15:14:08.905113-07:00
description: 'Comment faire : .'
lastmod: '2024-03-13T22:44:57.700927-06:00'
model: unknown
summary: .
title: Obtenir la date actuelle
weight: 29
---

## Comment faire :
```Elm
-- Vous aurez besoin du paquet elm/time
import Time exposing (Posix)

-- Exemple de fonction pour obtenir le temps actuel (Posix)
obtenirDateActuelle : Task.Task Time.Error Posix
obtenirDateActuelle = Time.now

-- Utilisation de la fonction dans le cadre d'un programme Elm
type Msg
    = RecevoirDateActuelle Posix

subscription : Sub Msg
subscription =
    Time.every 1000 RecevoirDateActuelle

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        RecevoirDateActuelle newPosix ->
            ( { model | currentTime = newPosix }
            , Cmd.none
            )

-- Affichage de la timestamp dans le view, nécessite une conversion
view : Model -> Html Msg
view model =
    let
        dateString =
            Time.posixToMillis model.currentTime
            |> String.fromInt
    in
    Html.text ("Timestamp actuel: " ++ dateString)

```
Sortie échantillon : `Timestamp actuel: 1615322342819`

## Approfondissement
Historiquement, Elm a évolué pour inclure des fonctionnalités de date et d'heure avec le module `elm/time`. Des méthodes alternatives incluent l'utilisation de paquets tiers mais l'approche standard reste l'utilisation du module `Time`. Le type `Posix` représente le temps en Elm, ce qui garantit un format universel plutôt que des strings ou des timestamps basés sur des locales spécifiques.

## Voir Aussi
- Documentation officielle d'Elm time : https://package.elm-lang.org/packages/elm/time/latest/
- Guide sur les tasks en Elm : https://guide.elm-lang.org/effects/task.html
- Pour une conversion de date avancée, le paquet justinmimbs/date: https://package.elm-lang.org/packages/justinmimbs/date/latest/
