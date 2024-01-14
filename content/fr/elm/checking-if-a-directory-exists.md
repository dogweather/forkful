---
title:                "Elm: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Dans la programmation, il est important de s'assurer que les fichiers ou les dossiers que l'on souhaite utiliser existent avant de les manipuler. Cela peut éviter des erreurs et des problèmes lors de l'exécution du code. En utilisant Elm, on peut facilement vérifier si un dossier existe avant de continuer avec les opérations prévues.

## Comment faire

Pour vérifier si un dossier existe en Elm, il faut utiliser la fonction `dirExists` du module `FileSystem`. Cette fonction prend en paramètre le chemin du dossier à vérifier et renvoie un `Task` avec un `Result` indiquant si le dossier existe ou non.

Voici un exemple de code avec un dossier existant :

```
Elm.FileSystem.dirExists "chemin/vers/mon_dossier"
    |> Task.perform checkDirectory
  where
    checkDirectory result =
        case result of
            Err error ->
                Debug.log "Erreur: " error

            Ok exists ->
                if exists then
                    Debug.log "Le dossier existe !"
                else
                    Debug.log "Le dossier n'existe pas."
```

Et voici un exemple avec un dossier inexistant :

```
Elm.FileSystem.dirExists "chemin/vers/dossier/incorrect"
    |> Task.perform checkDirectory
  where
    checkDirectory result =
        case result of
            Err error ->
                Debug.log "Erreur: " error

            Ok exists ->
                if exists then
                    Debug.log "Le dossier existe !"
                else
                    Debug.log "Le dossier n'existe pas."
```

La sortie pour ces deux exemples serait respectivement "Le dossier existe !" et "Le dossier n'existe pas.".

## Approfondissement

La fonction `dirExists` utilise la bibliothèque JavaScript `fs-extra` pour interagir avec le système de fichiers. Cela signifie que la fonction peut ne pas fonctionner sur certains navigateurs ou environnements. Il est donc important de s'assurer que l'environnement dans lequel on exécute le code est compatible avec `fs-extra`.

Il est également possible d'utiliser la fonction `fileExists` du même module pour vérifier l'existence d'un fichier plutôt que d'un dossier.

## Voir aussi

- Documentation officielle d'Elm sur le module FileSystem : <https://package.elm-lang.org/packages/elm/file/latest/>

- Documentation de `fs-extra` : <https://www.npmjs.com/package/fs-extra>

- Liste des modules officiels d'Elm : <https://package.elm-lang.org/>