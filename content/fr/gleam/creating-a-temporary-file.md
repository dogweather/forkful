---
title:    "Gleam: Création d'un fichier temporaire"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Pourquoi

La création de fichiers temporaires est un élément essentiel pour de nombreux programmes. Ces fichiers, comme leur nom l'indique, sont destinés à être utilisés temporairement pour stocker des données ou exécuter des tâches spécifiques. Cela peut sembler superflu, mais leur utilisation peut grandement faciliter la gestion des données dans votre code.

## Comment faire

Pour créer un fichier temporaire en utilisant Gleam, vous devez d'abord importer le module `File` en haut de votre code:

```
import File
```

Ensuite, vous pouvez simplement utiliser la fonction `create_temporary/1` pour créer un nouveau fichier temporaire. Cette fonction prend en paramètre le nom du fichier et retourne un tuple contenant le chemin complet du fichier et son gestionnaire.

```
let {path, handler} = File.create_temporary("example.txt")
```

Vous pouvez ensuite écrire et lire des données dans ce fichier en utilisant les fonctions `write/2` et `read/1` du module `File`.

```
File.write(handler, "Bonjour")
let data = File.read(handler)
```

N'oubliez pas de fermer le gestionnaire de fichier une fois que vous avez terminé en utilisant la fonction `close/1`.

```
File.close(handler)
```

## Plongée en profondeur

L'une des principales raisons pour lesquelles la création de fichiers temporaires est importante est qu'elle vous permet de gérer efficacement les accès concurrents aux données. En effet, chaque processus aura son propre fichier temporaire, évitant ainsi les conflits lors de l'écriture ou de la lecture de données.

Il est également important de noter que la fonction `create_temporary/1` prend en compte tous les répertoires spécifiés dans la variable d'environnement `TMPDIR`. Ainsi, le chemin du fichier temporaire peut varier en fonction de l'environnement dans lequel votre programme est exécuté.

## Voir aussi
- [Documentation Gleam sur la création de fichiers temporaires](https://gleam.run/modules/file.html#create_temporary/1)
- [Article sur l'utilisation des fichiers temporaires en programmation](https://www.redhat.com/en/blog/whats-behind-creating-temporary-files-programming)