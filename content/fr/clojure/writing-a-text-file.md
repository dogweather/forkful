---
title:                "Clojure: Écrire un fichier texte"
programming_language: "Clojure"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/clojure/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Ecrire un fichier texte peut sembler être une tâche simple, mais cela peut être très utile pour les programmeurs. Cela peut être utilisé pour stocker des données, enregistrer des logs ou même pour communiquer avec d'autres programmes.

## Comment faire

Pour écrire un fichier texte en Clojure, nous pouvons utiliser la fonction `spit` qui prend deux arguments : le chemin vers le fichier et le contenu à écrire. Voici un exemple de code :

```Clojure
(spit "monfichier.txt" "Bonjour le monde !")
```

Cela créera un fichier texte appelé "monfichier.txt" avec le contenu "Bonjour le monde !" à l'intérieur. Vous pouvez également utiliser des variables pour écrire du contenu plus dynamique, comme dans l'exemple ci-dessous :

```Clojure
(def texte "Hello world!")
(spit "monfichier.txt" texte)
```

Maintenant, le contenu du fichier sera "Hello world!".

Pour lire le contenu d'un fichier texte, nous pouvons utiliser la fonction `slurp` qui prend un argument : le chemin vers le fichier. Voici un exemple :

```Clojure
(def contenu (slurp "monfichier.txt"))
```

Maintenant, la variable `contenu` contiendra le contenu du fichier "monfichier.txt". Vous pouvez également utiliser ces fonctions pour écrire et lire des fichiers csv, json ou tout autre format de fichier texte.

## Plongée en profondeur

En plus de `spit` et `slurp`, Clojure offre également d'autres fonctions pour travailler avec des fichiers texte telles que `spit-append` pour ajouter du contenu à un fichier existant, `file-seq` pour parcourir un répertoire et `sh` pour exécuter des commandes shell. Vous pouvez également travailler avec des fichiers compressés en utilisant les fonctions `zipfile` et `unzipfile`.

Il est également important de savoir que Clojure utilise l'encodage Unicode UTF-8 par défaut pour les fichiers texte. Cela signifie que vous pouvez écrire et lire des caractères spéciaux et des symboles provenant de différentes langues sans rencontrer de problèmes d'encodage.

## Voir aussi

- [Documentation officielle de la fonction `spit` de Clojure](https://clojuredocs.org/clojure.core/spit)
- [Tutoriel sur la manipulation de fichiers en Clojure](https://www.lispcast.com/manipuler-fichiers-clojure)
- [Exemples de manipulation de fichiers en Clojure](https://www.clojure-toolbox.com/dependencies/clojure-financial/file-manipulation)