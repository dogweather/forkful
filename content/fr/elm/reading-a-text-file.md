---
title:    "Elm: Lecture d'un fichier texte"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elm/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un programmeur débutant ou expérimenté, vous savez probablement que la lecture de fichiers texte est un élément important du développement de logiciels. Que vous souhaitiez récupérer des données à partir d'un fichier ou simplement le lire pour en vérifier le contenu, savoir comment le faire en Elm peut vous être très utile. Dans cet article, nous allons explorer les bases de la lecture de fichiers texte en Elm.

## Comment faire

Pour lire un fichier texte en Elm, nous utiliserons la fonction `Text.readFile`. Cette fonction prend en paramètre le chemin du fichier à lire et renvoie une `Task` contenant le contenu du fichier. Voici un exemple de code :

```
import Text exposing (readFile)

main =
  readFile "mon_fichier.txt" 
```

Dans cet exemple, nous utilisons la fonction `readFile` pour lire le fichier texte "mon_fichier.txt". Cette fonction renvoie une `Task` que nous pouvons manipuler à l'aide de la fonction `Task.attempt`. Nous pouvons ensuite utiliser cette `Task` pour afficher le contenu du fichier ou le stocker dans une variable pour une utilisation ultérieure. Voici un exemple de code complet :

```
import Html exposing (text)
import Task exposing (attempt)
import Text exposing (readFile)

main =
  Task.attempt ReadFile (readFile "mon_fichier.txt")

type Msg
  = ReadFile (Result String Error)

type alias Error =
  { message : String
  , code : Int
  }

update msg model =
  case msg of
    ReadFile result ->
      case result of
        Ok content ->
          -- faire quelque chose avec le contenu du fichier
          text content
        Err error ->
          -- gérer l'erreur d'ouverture du fichier
          text error.message
```

## Plongée profonde

La fonction `Text.readFile` prend en charge les chemins relatifs et absolus pour spécifier le fichier à lire. Il existe également la fonction `Text.get` qui prend en charge les demandes HTTP pour récupérer un fichier en ligne. De plus, il est important de noter que la `Task` renvoyée par `Text.readFile` est asynchrone, ce qui signifie que nous devons utiliser la fonction `Task.attempt` pour manipuler les résultats.

## Voir aussi

- Documentation officielle sur la fonction `Text.readFile`: https://package.elm-lang.org/packages/elm/core/latest/Text#readFile
- Documentation officielle sur la fonction `Task.attempt`: https://package.elm-lang.org/packages/elm/core/latest/Task#attempt
- Tutoriel sur la lecture et l'écriture de fichiers en Elm: https://guide.elm-lang.org/io/files.html