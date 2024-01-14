---
title:                "Elm: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Elm"
category:             "Elm"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elm/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Pourquoi

La création de fichiers temporaires est une tâche importante en programmation, que ce soit pour stocker temporairement des données ou pour effectuer des opérations sur des fichiers volumineux. Dans cet article, nous allons explorer comment créer des fichiers temporaires en utilisant la langage de programmation Elm.

# Comment faire

Pour créer un fichier temporaire en Elm, nous allons utiliser la fonction `File.temp`. Cette fonction prend en argument le chemin d'accès au fichier temporaire et le contenu que vous souhaitez y écrire. Voici un exemple de code :

```Elm
file : String
file = "/mon/chemin/fichier/temporaire"

contenu : String
contenu = "Ceci est un contenu temporaire"

_ = File.temp file contenu
```

En exécutant ce code, un nouveau fichier temporaire sera créé dans le chemin spécifié et le contenu sera écrit à l'intérieur de celui-ci. Vous pouvez également spécifier des options supplémentaires telles que les droits d'accès du fichier ou sa position dans le système de fichiers.

# Plongée en profondeur

La fonction `File.temp` utilise une autre fonction appelée `File.openTempFile`. Cette fonction prend en argument un gestionnaire de fichiers et renvoie un chemin d'accès au fichier temporaire créé. En utilisant cette fonction, vous pouvez avoir plus de contrôle sur la façon dont le fichier temporaire est créé et géré.

Cependant, il est important de noter que les fichiers temporaires sont automatiquement supprimés une fois que votre programme a fini de s'exécuter. Donc, si vous avez besoin de conserver les données du fichier temporaire pendant une période plus longue, vous devrez enregistrer le fichier dans un emplacement permanent avant que le programme ne se termine.

# Voir aussi

- [Documentation officielle Elm - File](https://package.elm-lang.org/packages/elm/file/latest/)
- [Tutoriel sur la manipulation de fichiers en Elm](https://dev.to/baransu/file-listing-and-manipulation-in-elm-43ja)
- [Article sur la gestion des fichiers temporaires en Elm](https://medium.com/swlh/temporary-files-management-in-elm-47edb9509201)