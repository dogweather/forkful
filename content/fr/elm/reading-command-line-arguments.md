---
title:                "Elm: Lecture des arguments de ligne de commande"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi
Les arguments de la ligne de commande sont utilisés dans de nombreuses applications pour personnaliser le fonctionnement du programme. L'apprentissage de la lecture des arguments de ligne de commande en Elm peut être utile pour développer des applications plus flexibles et polyvalentes.

## Comment faire
Pour lire les arguments de ligne de commande en Elm, il suffit d'utiliser la fonction `CommandLine.arguments` qui renvoie une liste de `String` représentant les arguments passés au programme. Voici un exemple de code:

```Elm
-- Lecture des arguments de la ligne de commande
main =
    CommandLine.arguments
        |> List.map Console.log
```

Si vous exécutez ce code avec les arguments `elm-make MyFile.elm`, la sortie sera une liste contenant `[ "elm-make", "MyFile.elm" ]`.

## En profondeur
Outre la lecture des arguments de ligne de commande, il est également possible d'utiliser le module `CommandLine` pour définir des options avec des valeurs par défaut et des alias d'options. Cela peut être utile pour fournir des options de configuration à votre programme. Voici un exemple de code utilisant `CommandLine`.

```Elm
import CommandLine exposing (Command)

type alias Config =
    { verbose : Bool
    , port : Int
    }

-- Définition de l'option "--verbose" avec un alias "-v"
verboseOption : Command Config
verboseOption =
  CommandLine.flagWithAliases
    { aliases = [ "v" ]
    , help = "Enable verbose mode."
    , value = .verbose
    , defaultValue = False
    }

-- Définition de l'option "--port" avec une valeur par défaut
portOption : Command Config
portOption =
  CommandLine.option
    { longName = "port"
    , help = "Specify which port to run the server on."
    , argument = CommandLine.int
    , value = .port
    , defaultValue = 8080
    }

-- Fonction principale utilisant les options définies
main : CommandLine.ArgParser () Config
main =
    CommandLine.mapConfig Config
        |> CommandLine.addOption portOption
        |> CommandLine.addCommand verboseOption

```

Avec cette définition, vous pouvez maintenant exécuter votre programme en fournissant des options telles que `./MyProgram --verbose --port 3000`. Les options seront automatiquement parsées et la configuration correspondante sera renvoyée.

# Voir aussi
- [Documentation Elm sur la lecture des arguments de ligne de commande](https://package.elm-lang.org/packages/elm/core/latest/CommandLine)
- [Guide Elm sur la gestion des paramètres et des options de ligne de commande](https://guide.elm-lang.org/managing_program_arguments.html)