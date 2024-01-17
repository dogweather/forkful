---
title:                "Vérifier si un répertoire existe"
html_title:           "Elm: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?
Vérifier si un répertoire existe est une tâche courante pour les programmeurs Elm. Cela permet de s'assurer qu'un chemin d'accès spécifique est valide avant d'y accéder. 

## Comment faire:
Voici un exemple de code pour vérifier si le répertoire "documents" existe :

```Elm
import File
import Maybe exposing (withDefault)

directoryExists : String -> Cmd Msg
directoryExists directory =
    File.exists directory
        |> Task.perform (Always (DirectoryExists directory))

-- Output : DirectoryExists "documents"
```

## Plongée en profondeur:
Lorsqu'Elm a été créé en 2012, la plupart des autres langages de programmation n'offraient pas une syntaxe simple pour vérifier si un répertoire existe. Cela a donc été une fonctionnalité importante pour faciliter le développement en Elm. 

Une alternative à la vérification d'un répertoire serait de simplement essayer d'accéder au répertoire sans le vérifier au préalable. Cependant, cela pourrait entraîner des erreurs inattendues si le répertoire n'existe pas. De plus, la vérification du répertoire assure une meilleure organisation du code et une plus grande robustesse du programme. 

L'implémentation de la vérification d'un répertoire en Elm utilise la fonction existant dans le module File, qui renvoie une tâche contenant un booleen indiquant si le répertoire existe ou non. Si vous voulez simplement vérifier si un répertoire existe sans utiliser le résultat de la tâche, vous pouvez utiliser la fonction Task.perform pour effectuer la tâche avec la commande Cmd, comme indiqué dans l'exemple de code ci-dessus.

## Voir aussi:
Pour plus d'informations sur la vérification de l'existence d'un répertoire en Elm, vous pouvez consulter la documentation officielle du module File et découvrir d'autres fonctions utiles pour travailler avec des fichiers et des répertoires.