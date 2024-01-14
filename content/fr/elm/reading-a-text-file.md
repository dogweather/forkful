---
title:    "Elm: Lecture d'un fichier texte"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Pourquoi
Les fichiers texte jouent un rôle important dans la programmation, que ce soit pour stocker des données ou pour lire des informations externes dans une application Elm. Il est donc essentiel de savoir comment lire un fichier texte efficacement dans ce langage de programmation fonctionnel.

## Comment faire
Heureusement, Elm fournit une fonctionnalité très pratique pour lire les fichiers texte : la fonction `Text.fromString`. Elle prend en paramètre une chaîne de caractères correspondant au chemin du fichier et retourne un `Maybe String` qui contient le contenu du fichier. Voyons un exemple d'utilisation avec la fonction `Debug.log`, qui permet d'afficher une valeur dans la console :

```
import File
import Debug

main : Program Never Model Msg
main =
  Debug.log "Contenu du fichier :" (File.Text.fromString "monFichier.txt")
```

Si le fichier `monFichier.txt` contient par exemple "Bonjour Elm!", le résultat affiché dans la console sera :

```
"Contenu du fichier :" Just "Bonjour Elm!"
```

Notez que la valeur retournée est emballée dans un `Maybe`, car la fonction `Text.fromString` peut échouer si le chemin du fichier est incorrect ou si le fichier n'existe pas.

## Plongée en profondeur
Il est également possible de spécifier l'encodage du fichier lors de la lecture en utilisant la fonction `Text.fromBytes`. Cette fonction prend en paramètre un `Encoding` et une chaîne de caractères correspondant au chemin du fichier. Elle retourne également un `Maybe String` avec le contenu du fichier, mais cette fois-ci encodé selon le format choisi.

Par exemple, si nous voulons lire un fichier encodé en UTF-8, nous pouvons utiliser la fonction `Text.UTF8` pour spécifier l'encodage :

```
import File
import Debug
import Text

main : Program Never Model Msg
main =
  Debug.log "Contenu du fichier :" (File.Text.fromBytes Text.UTF8 "monFichier.txt")
```

## Voir aussi
- Documentation officielle sur la lecture de fichiers : https://elm-lang.org/docs/unsorted/files
- Tutoriel sur les `Maybe` : https://elmprogramming.com/maybe-values.html
- Exemple d'application utilisant la lecture de fichiers : https://github.com/killianF/elm-csv-parser