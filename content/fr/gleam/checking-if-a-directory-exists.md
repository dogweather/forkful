---
title:    "Gleam: Vérifier si un répertoire existe"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

La vérification de l'existence d'un répertoire est une étape importante dans de nombreux projets de programmation. Cela permet de s'assurer que le répertoire dans lequel vous souhaitez enregistrer ou lire des fichiers existe bien et évite ainsi les erreurs inattendues lors de l'exécution du code.

## Comment faire

Pour vérifier si un répertoire existe en utilisant le langage de programmation Gleam, vous pouvez utiliser la fonction `exists` du module `gleam_fs`. Voici un exemple de code avec un répertoire existant :

```
gleam_fs.exists("chemin/du/repertoire")
# => true
```

Et un exemple avec un répertoire inexistant :

```
gleam_fs.exists("chemin/vers/repertoire/inexistant")
# => false
```

## Plongeons plus en profondeur

La fonction `exists` fonctionne en utilisant la bibliothèque standard du système d'exploitation sur lequel Gleam est exécuté. Cela signifie qu'elle est sensible à la façon dont le système d'exploitation gère les chemins et les fichiers. Par exemple, sous Windows, les chemins utilisent des barres obliques inversées `\` tandis que sous Linux, ils utilisent des barres obliques normales `/`.

Il est également important de noter que la fonction `exists` peut également être utilisée pour vérifier l'existence de fichiers, et pas seulement de répertoires.

## Voir aussi

- [Documentation du module gleam_fs](https://gleam.run/modules/io/fs/)
- [Tutoriel sur la gestion de fichiers en Gleam](https://medium.com/@gleamlangio/file-handling-in-gleam-63ceaa558920)
- [GitHub du langage Gleam](https://github.com/gleam-lang/gleam)