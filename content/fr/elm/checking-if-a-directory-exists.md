---
title:    "Elm: Vérifier si un répertoire existe"
keywords: ["Elm"]
---

{{< edit_this_page >}}

# Pourquoi

Vous êtes probablement en train de vous demander pourquoi vous devriez perdre du temps à vérifier si un répertoire existe. Eh bien, c'est une étape importante pour s'assurer que votre code est robuste et peut gérer toutes les situations. Il est également utile de savoir si un répertoire existe avant de tenter d'y accéder ou d'y créer des fichiers.

# Comment faire

Heureusement, Elm rend cette tâche assez simple grâce à la fonction `Directory.doesExist`. Voici un exemple de code qui vérifie si un répertoire existe et imprime un message en conséquence :

```Elm
import Directory

main =
    Directory.doesExist "/chemin/vers/mon/repertoire"
        |> Task.map (\exists -> 
            if exists then 
                "Le répertoire existe"
            else
                "Le répertoire n'existe pas"
            )
        |> Task.perform (Debug.log "Etat du répertoire : ") 
```

Dans cet exemple, nous utilisons `Task.map` pour effectuer une action en fonction du résultat de `Directory.doesExist`. Si le répertoire existe, nous imprimons un message, sinon, un autre message est affiché.

# Plongeon en profondeur

Maintenant que nous savons comment vérifier si un répertoire existe en Elm, explorons un peu plus en détails cette fonction. Elle prend en paramètre un chemin vers un répertoire sous forme de chaîne de caractères et renvoie une tâche qui se résout avec un booléen indiquant si le répertoire existe ou non.

Il est important de noter que cette fonction ne vérifie pas si le chemin est valide ou si c'est bien un répertoire. Elle se contente de vérifier si le répertoire existe à l'endroit spécifié. Si vous souhaitez également vérifier si le chemin est valide, vous devrez utiliser la fonction `Directory.isDir`.

# Voir aussi

- La documentation officielle sur `Directory.doesExist` : https://package.elm-lang.org/packages/elm/filesystem/latest/Directory#doesExist
- Un tutoriel sur la gestion des fichiers en Elm : https://dev.to/simonh1000/elm-guide-to-file-handling-1h63
- Un article sur les tâches en Elm : https://medium.com/@robinhuy/lindispensable-des-t%C3%A2ches-en-elm-e9e3d40bd54f