---
title:                "Journalisation"
aliases:
- /fr/elm/logging/
date:                  2024-01-26T01:02:48.560565-07:00
model:                 gpt-4-1106-preview
simple_title:         "Journalisation"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/logging.md"
---

{{< edit_this_page >}}

## Quoi et pourquoi ?
Le logging est essentiellement le processus d'enregistrement des événements et des sorties de données d'un logiciel lors de son exécution, pensez-y comme au journal intime du logiciel. Les programmeurs utilisent les logs pour suivre ce qui se passe sous le capot - cela est inestimable pour le débogage des problèmes, le suivi du comportement du système en temps réel, et l'analyse de l'activité passée pour des optimisations de performance ou des audits.

## Comment faire :
L'architecture d'Elm ne prend pas en charge les effets secondaires comme le logging par défaut - vous les gérez à travers des commandes, qui font partie de l'architecture de votre application. À des fins pédagogiques, voyons comment vous pourriez simuler un logging en envoyant des messages à JavaScript via des ports.

D'abord, vous définirez un module port :

```Elm
port module Logger exposing (..)

-- Définir un port pour envoyer les logs à JavaScript
port log : String -> Cmd msg
```

Dans votre `Main.elm`, vous utiliseriez le port `log` pour envoyer un message de log :

```Elm
import Logger exposing (log)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        AnEvent ->
            -- quelques mises à jour de votre modèle ici
            ( updatedModel, log "Un AnEvent s'est produit." )

        AnotherEvent ->
            -- d'autres mises à jour du modèle ici
            ( anotherUpdatedModel, log "Un AnotherEvent s'est produit." )
```

Du côté JavaScript, vous vous abonneriez au port `log` pour gérer les messages de log entrants :

```JavaScript
var app = Elm.Main.init({ /* ... */ });

app.ports.log.subscribe(function(message) {
    console.log(message);
});
```

Un exemple de sortie dans la console JavaScript serait alors :

```
Un AnEvent s'est produit.
Un AnotherEvent s'est produit.
```

## Exploration en profondeur
Traditionnellement, dans des langages comme Python ou Java, le logging est effectué en utilisant une bibliothèque de logging, qui fournit une API simple pour enregistrer des messages à divers niveaux tels que debug, info, warning, error et critical.

Elm, avec son accent sur la pureté et l'immutabilité, ne fournit pas de logging direct de ce type, car tout type d'E/S ou effet secondaire est géré de manière distincte à travers l'architecture Elm.

Quand vous avez besoin d'un logging complet avec Elm, vous vous reposez habituellement sur des outils JavaScript externes. Les ports, comme montré précédemment, sont le pont vers ces outils. Le module Debug est une autre option, mais il est destiné uniquement à être utilisé en développement et non pour le logging en production.

En plus des ports, les programmeurs font souvent usage des messages du compilateur Elm et des facilités de débogage à l'exécution, comme `Debug.log`, que vous pouvez insérer dans votre code pour tracer des valeurs. Il enveloppe une expression et enregistre sa sortie dans la console comme ceci :

```Elm
view model =
    Debug.log "Débogage du modèle" model
    -- votre code de vue ici
```

Cela aussi n'est cependant pas destiné à la production. Des outils comme elm-logger fournissent quelques abstractions par-dessus les ports pour le logging, bien que ceux-ci soient également plutôt destinés au développement qu'à la production.

## Voir aussi
- Ports Elm : https://guide.elm-lang.org/interop/ports.html
- Elm `Debug` : https://package.elm-lang.org/packages/elm/core/latest/Debug
- Discours sur le logging avec Elm : https://discourse.elm-lang.org/t/elm-and-logging/546
- API Console JavaScript : https://developer.mozilla.org/fr/docs/Web/API/Console
- Package elm-logger : https://package.elm-lang.org/packages/arkgil/elm-logger/latest/
