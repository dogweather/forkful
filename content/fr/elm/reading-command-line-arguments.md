---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Lecture d'arguments de ligne de commande en Elm

## Quoi & Pourquoi?
La lecture des arguments de ligne de commande permet de recevoir des données directement du terminal lors de l'exécution d'un programme. Les programmeurs utilisent cette méthode pour manipuler, personnaliser, ou contrôler le comportement des applications selon les paramètres passés à l'ouverture.

## Comment faire:
Notez qu'Elm est conçu pour créer des applications front-end, donc la lecture d'arguments de ligne de commande n'est généralement pas une pratique courante en Elm. Toutefois, vous pouvez le faire en utilisant JavaScript par l'intermédiaire des ports. Voici comment vous pouvez le faire:

```Elm
port module Main exposing (..)

port readCommandLineArguments : () -> Cmd msg

main =
    readCommandLineArguments ()
```
Dans votre fichier JavaScript:
```JavaScript
var node = Elm.Main.init();
node.ports.readCommandLineArguments.send(process.argv);
```
Lorsque vous exécutez votre code Elm à partir de Node.js, `process.argv` envoie les arguments de la ligne de commande à Elm.

## Plongée profonde:
Historiquement, la gestion d'arguments de la ligne de commande est une fonctionnalité présente dans de nombreux langages de programmation tels que C, Python, et JavaScript. Cependant, puisque Elm est principalement utilisé pour la programmation front-end, cette fonctionnalité n'est pas intégrée dans le langage lui-même.

En ce qui concerne les alternatives, vous pouvez utiliser Flag avec `Browser.application` pour passer des données à Elm lors de l'initialisation ou recourir à l'approche démontrée précédemment.

En ce qui concerne l'implémentation, l'utilisation des ports est nécessaire parce qu'Elm est un langage pur, ce qui signifie qu'il évite les effets secondaires non contrôlés comme la lecture directe de la ligne de commande.

## Voir Aussi:
Plus d'informations sur les ports en Elm peuvent être trouvées ici:

- https://guide.elm-lang.org/interop/ports.html
- http://elmprogramming.com/ports.html

Et pour une discussion plus générale sur la gestion des arguments de la ligne de commande dans d'autres langages:

- https://en.wikipedia.org/wiki/Command-line_argument_parsing