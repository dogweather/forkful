---
title:                "Gleam: Vérification de l'existence d'un répertoire"
programming_language: "Gleam"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Si vous êtes un développeur Gleam, vous savez probablement qu'il est important de vérifier si un répertoire existe avant de tenter de le manipuler dans votre code. Mais pourquoi est-il si important de le faire ?

La principale raison est de s'assurer que votre code ne se heurte à des erreurs ou des exceptions en essayant de manipuler un répertoire inexistant. Cela peut également vous faire gagner du temps en évitant de traiter des erreurs inutiles et en vous assurant que votre code fonctionne correctement dès le départ.

# Comment faire

Heureusement, vérifier l'existence d'un répertoire en Gleam est assez simple. Vous pouvez utiliser la fonction `FileSystem.exists/1` en spécifiant le chemin du répertoire que vous voulez vérifier. Voici un exemple de code :

```Gleam
import gleam/fs
let directory = "./mon_repertoire/"
let exists = fs.exists(directory)
```

Dans cet exemple, nous importons le module `gleam/fs` qui contient la fonction `exists/1` dont nous avons besoin. Nous définissons ensuite une variable `directory` contenant le chemin du répertoire que nous voulons vérifier. Enfin, nous utilisons la fonction `exists/1` pour vérifier si ce répertoire existe et stockons le résultat dans la variable `exists`.

Si le répertoire existe, la variable `exists` vaudra `true`, sinon elle vaudra `false`. Vous pouvez ensuite utiliser cette information dans votre code pour décider de la suite des opérations à effectuer.

# Plongée en profondeur

Si vous souhaitez en savoir plus sur la vérification de l'existence d'un répertoire en Gleam, voici quelques points à retenir :

- La fonction `exists/1` de `gleam/fs` renvoie un `Result` contenant un `Ok` si le répertoire existe, ou un `Err` si une erreur s'est produite.
- Vous pouvez également utiliser la fonction `exists_file/1` si vous voulez vérifier l'existence d'un fichier plutôt que d'un répertoire.
- Si vous travaillez avec un répertoire spécifique, vous pouvez préciser un filtre en utilisant la fonction `FileSystem.list/2` pour obtenir une liste des fichiers et dossiers dans ce répertoire. Vous pouvez ensuite parcourir cette liste pour vérifier si un certain fichier ou dossier existe.

# Voir aussi

- Documentation officielle sur la fonction `FileSystem.exists/1` : https://gleam.run/modules/gleam_fs#exists/1
- Tutoriel sur la manipulation des fichiers et répertoires en Gleam : https://gleam.run/book/tutorials/files.html