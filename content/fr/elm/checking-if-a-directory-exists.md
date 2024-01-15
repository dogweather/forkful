---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Elm: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi 
Si vous êtes un développeur Elm, il est probable que vous ayez rencontré le besoin de vérifier si un dossier existe avant de continuer l'exécution de votre programme. Cela peut sembler être une tâche facile, mais c'est en fait un peu plus complexe que ça. Dans cet article, nous allons vous montrer comment le faire en utilisant Elm.

## Comment faire 
```Elm
import File exposing (exists)
import File.System.Path as Path

checkDirectoryExists : String -> Task x Bool
checkDirectoryExists directory =
    exists (Path.directory directory)
```

Voici un exemple de fonction en Elm qui prend en paramètre le chemin d'un dossier et renvoie une tâche qui se termine par un booléen indiquant si le dossier existe ou non. Nous utilisons la fonction `exists` de la bibliothèque `File` et nous lui passons le chemin du dossier en utilisant la fonction `directory` de la bibliothèque `File.System.Path`.

Maintenant, si vous souhaitez utiliser cette fonction dans votre application, voici comment vous pouvez le faire :

```Elm
import Task
import Task.Extra
import Platform
import Html exposing (text)

main : Program Never
main =
    Program.none
        { init = ( Model "my_directory", Cmd.none )
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }

type alias Model =
    { directory : String
    , exists : Bool
    }

type Msg
    = DirectoryExists (Result x Bool)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DirectoryExists result ->
            case result of
                Result.Ok bool ->
                    ( { model | exists = bool }, Cmd.none )

                Result.Err _ ->
                    ( { model | exists = False }, Cmd.none )

view : Model -> Html Msg
view model =
    text (toString model.exists)

subscriptions : Model -> Sub Msg
subscriptions model =
    if not model.exists then
        Sub.batch
            [ Task.Extra.attempt DirectoryExists (checkDirectoryExists model.directory) ]
    else
        Sub.none
```

Nous créons une application Elm simple avec un modèle qui contient le chemin du dossier et un booléen pour indiquer s'il existe ou non. Dans le gestionnaire de mise à jour, nous utilisons la fonction `checkDirectoryExists` pour vérifier si le dossier existe et si oui, nous mettons à jour notre modèle avec la valeur renvoyée. Dans le gestionnaire de vue, nous affichons simplement la valeur booléenne.

## Plongée en profondeur 
Maintenant que vous avez vu comment vérifier si un dossier existe en utilisant Elm, il est important de comprendre que cela ne fonctionne que pour les dossiers qui se trouvent sur le système de fichiers de l'utilisateur. Cela signifie que vous ne pourrez pas vérifier l'existence de dossiers sur un serveur distant, par exemple.

De plus, la fonction `exists` ne prend en compte que les dossiers et non les fichiers. Si vous souhaitez vérifier l'existence d'un fichier, vous devrez utiliser une autre fonction de la bibliothèque `File`, comme `FileInfo.exists`.

## Voir aussi 
- [Documentation sur la bibliothèque File d'Elm](https://package.elm-lang.org/packages/elm/file/latest/)
- [Documentation sur la bibliothèque File.System.Path d'Elm](https://package.elm-lang.org/packages/elm/file/latest/File-System-Path)
- [Exemple d'utilisation de la fonction `FileInfo.exists`](https://github.com/elm/file/blob/master/examples/existingFile.elm)