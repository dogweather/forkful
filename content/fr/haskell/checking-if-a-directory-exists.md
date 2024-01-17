---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Haskell: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Qu'est-ce que c'est et pourquoi faire ?

Vérifier si un répertoire existe est une opération courante utilisée par les programmeurs pour s'assurer qu'un chemin de fichier est valide avant de procéder à d'autres manipulations, telles que la création d'un nouveau fichier ou l'ouverture d'un fichier existant.

# Comment faire :

Voici un exemple de code montrant comment vérifier si un répertoire existe en Haskell :

```Haskell
import System.Directory

main = do
    let path = "/chemin/vers/le/repertoire"
    dirExists <- doesDirectoryExist path
    if dirExists
        then putStrLn "Le répertoire existe !"
        else putStrLn "Le répertoire n'existe pas."
```

Si le répertoire existait, le programme afficherait "Le répertoire existe !". Sinon, il afficherait "Le répertoire n'existe pas."

# Zoom sur :

## Contexte historique :
La vérification de l'existence d'un répertoire a toujours été une tâche importante en informatique, car les programmes doivent savoir si une ressource est disponible avant de pouvoir l'utiliser.

## Alternatives :
Il existe différentes façons de vérifier si un répertoire existe, notamment en utilisant des bibliothèques externes ou en utilisant des fonctions spécifiques au système d'exploitation.

## Détails de mise en œuvre :
En Haskell, la fonction "doesDirectoryExist" de la bibliothèque System.Directory est utilisée pour vérifier si un répertoire existe. Elle renvoie un type de données de type Bool (True ou False) en fonction du résultat.

# Voir aussi :

- [Documentation de la fonction doesDirectoryExist de la bibliothèque System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html#v:doesDirectoryExist)
- [Tutoriel sur la manipulation de fichiers en Haskell](https://www.tutorialspoint.com/haskell/haskell_files_io.htm)