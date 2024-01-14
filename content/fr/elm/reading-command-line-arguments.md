---
title:    "Elm: Lecture des arguments de ligne de commande"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un programmeur Elm, vous savez probablement déjà comment lire les arguments de ligne de commande. Cependant, si vous êtes débutant en Elm ou si vous souhaitez simplement rafraîchir vos connaissances, cet article est pour vous ! La lecture des arguments de ligne de commande est une compétence utile dans de nombreux projets, donc cela vaut la peine d'apprendre comment le faire correctement.

# Comment procéder

Tout d'abord, importez le module "Platform.CmdLine" dans votre fichier Elm. Cela nous permet d'utiliser les fonctions fournies par ce module pour lire les arguments de ligne de commande. Une fois que cela est fait, nous allons utiliser la fonction "get" pour obtenir les arguments en tant que chaîne de caractères.

```
import Platform.CmdLine exposing (get)

main =
    get
        |> Task.perform CmdLineResult
```

En utilisant "Task.perform", nous pouvons extraire les résultats de la tâche en utilisant la fonction "CmdLineResult". Ce résultat sera stocké dans une variable appelée "args", que nous pouvons utiliser pour accéder aux arguments de ligne de commande.

```
import Platform.CmdLine exposing (get)

main =
    get
        |> Task.perform CmdLineResult

type alias CmdLineResult =
    Result String String

args =
    case args of
        Ok arguments ->
            -- faire quelque chose avec les arguments ici

        Err error ->
            Debug.log error "Erreur lors de la lecture des arguments de ligne de commande"
```

Maintenant que nous avons accès aux arguments, nous pouvons les utiliser dans notre programme pour effectuer différentes tâches en fonction de ce qui est passé en ligne de commande. Par exemple, si vous souhaitez afficher un message différent en fonction du premier argument, vous pouvez utiliser un pattern matching pour extraire l'argument et afficher un message différent en fonction de sa valeur.

```
import Platform.CmdLine exposing (get)
import Html exposing (..)

main =
    get
        |> Task.perform CmdLineResult

type alias CmdLineResult =
    Result String String

args =
    case args of
        Ok arguments ->
            case arguments of
                "bonjour" ->
                    text "Bonjour !"

                "au revoir" ->
                    text "Au revoir !"

                _ ->
                    text "Argument inconnu. Veuillez entrer 'bonjour' ou 'au revoir'."

        Err error ->
            Debug.log error "Erreur lors de la lecture des arguments de ligne de commande"
```

# Plongée en profondeur

Maintenant que vous savez comment lire les arguments de ligne de commande en Elm, il peut être utile de comprendre comment cela fonctionne réellement en coulisses. En utilisant la fonction "get", nous obtenons une tâche qui renvoie un "Result String String". Cette tâche est exécutée par la fonction "Task.perform", qui prend deux arguments : une fonction et une tâche. La fonction spécifiée est appelée avec le résultat de la tâche en tant qu'argument, ce qui nous permet d'accéder aux arguments de ligne de commande dans notre programme.

# Voir aussi

Maintenant que vous savez comment lire les arguments de ligne de commande en Elm, voici quelques liens utiles pour poursuivre votre apprentissage :

- [Documentation du module Platform.CmdLine](https://package.elm-lang.org/packages/elm-lang/core/latest/Platform-CmdLine)
- [Vidéo sur la lecture des arguments de ligne de commande en Elm](https://www.youtube.com/watch?v=N3_qFBbdpUc)
- [Exemple de code utilisant la lecture des arguments de ligne de commande en Elm](https://github.com/elm-community/elm-depth)