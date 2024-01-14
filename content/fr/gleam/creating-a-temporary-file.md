---
title:    "Gleam: Création d'un fichier temporaire"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Pourquoi créer un fichier temporaire avec Gleam

Créer un fichier temporaire est une tâche courante dans la programmation. Cela peut être utile pour stocker des données temporaires ou pour effectuer des opérations sur des fichiers sans les modifier directement. Dans cet article, nous allons vous expliquer comment utiliser Gleam pour créer des fichiers temporaires efficacement.

## Comment créer un fichier temporaire en Gleam

Pour créer un fichier temporaire en Gleam, nous allons utiliser la fonction `TempFile.create/0` de la bibliothèque standard `gleam/io`. Cette fonction prendra en paramètre le chemin d'accès au fichier temporaire. Voici un exemple de code qui crée un fichier temporaire et y écrit "Bonjour le monde !" :

```Gleam
import gleam/io as io

let file = io.TempFile.create("/chemin/vers/mon/fichier/temp")
io.write(file, "Bonjour le monde !")
```

Ensuite, nous pouvons vérifier que le fichier a bien été créé en lisant son contenu à l'aide de la fonction `io.read_all/1` :

```Gleam
let content = io.read_all(file)
io.print(content) // imprime "Bonjour le monde !"
```

## Plongée en profondeur

Lorsque nous créons un fichier temporaire, il est important d'utiliser un chemin d'accès unique pour éviter les conflits avec d'autres fichiers. Gleam offre une fonction `io.TempFile.create_unique/0` qui génère un chemin d'accès unique à chaque appel. De plus, il est recommandé de supprimer le fichier temporaire après son utilisation pour éviter d'encombrer le système de fichiers.

Voici un exemple de code qui utilise ces deux fonctionnalités :

```Gleam
import gleam/io as io

let file = io.TempFile.create_unique()
io.write(file, "Bonjour le monde !")
let content = io.read_all(file)
io.print(content) // imprime "Bonjour le monde !"
io.remove(file) // supprime le fichier temporaire
```

En utilisant ces techniques, vous pourrez créer et utiliser des fichiers temporaires en toute sécurité dans votre code Gleam.

# Voir aussi

Voici quelques liens utiles pour en savoir plus sur la création de fichiers temporaires en Gleam :

- Documentation officielle de Gleam : https://gleam.run/
- Documentation de la bibliothèque standard de Gleam : https://gleam.run/modules/gleam
- Tutoriel sur la création de fichiers temporaires en Gleam : [lien vers le tutoriel en anglais]