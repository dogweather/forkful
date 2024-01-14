---
title:                "Elm: Vérifier si un répertoire existe"
programming_language: "Elm"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Il est fréquent que les programmes Elm aient besoin de savoir si un dossier existe avant de pouvoir y accéder ou de le créer. En vérifiant si un dossier existe au préalable, vous pouvez éviter les erreurs et gérer les cas où le dossier n'existe pas encore.

## Comment faire

Cela peut sembler compliqué, mais avec Elm, vérifier si un dossier existe est en fait assez simple. Vous avez besoin de deux choses : le nom du dossier que vous voulez vérifier et le chemin du dossier.

```Elm
existeDossier : String -> String -> Cmd Msg
existeDossier nom chemin =
  Native.Directory.exists nom chemin toMsg
```

La fonction `existeDossier` prend en paramètres le nom du dossier et le chemin du dossier, puis renvoie une commande `Cmd` avec un message à traiter. Pour utiliser cette fonction, vous pouvez l'invoquer dans votre `update` comme suit :

```Elm
case msg of
  ...
  VerifierDossier ->
    existeDossier "documents" "dossier/parent"
  ...
```

Ensuite, vous pouvez gérer le résultat de la vérification dans votre `init` en ajoutant un `case` supplémentaire :

```Elm
init : Model
init =
  { nomDossier = ""
  , cheminDossier = ""
  }

type Msg
  = ...
  | ResultatVerification Bool

toMsg : Platform.Task Never bool -> Task Msg Bool 
toMsg task =
  fromA2 ResultatVerification task

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    ...
    ResultatVerification existe ->
      ({ model | nomDossier = "documents"
               , cheminDossier = "dossier/parent"
        }, Cmd.none)
    ...
```

## Plongée en profondeur

La fonction `existeDossier` utilise des fonctions natives pour interagir avec le système de fichiers. Cela peut poser des problèmes de sécurité et de confidentialité, c'est pourquoi ces fonctions sont disponibles dans un module distinct appelé `Native.Directory`.

Il est également important de noter que cette fonction ne vérifie que l'existence d'un dossier et pas sa validité. Vous devriez également vérifier si le dossier est vide ou s'il contient des fichiers avant d'y accéder ou de le supprimer.

## Voir aussi

- [Documentation officielle Elm on directories](https://guide.elm-lang.org/interop/directories.html)
- [Managing File and Folder Paths in Elm](https://thoughtbot.com/blog/managing-file-and-folder-paths-in-elm)
- [Filesystem Access in Elm with Elmap](https://www.elmap.org/2020/02/filesystem-access-in-elm/)