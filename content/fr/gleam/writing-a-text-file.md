---
title:                "Écrire un fichier texte"
html_title:           "Gleam: Écrire un fichier texte"
simple_title:         "Écrire un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi
Il est important de savoir comment écrire un fichier texte en programmation afin de stocker et de manipuler des données de manière structurée. Cela peut être utile pour créer des applications, générer des rapports et bien plus encore.

## Comment faire

```Gleam
let mon_texte = "Voici mon fichier texte"

let fichier = File.write("mon_fichier.txt", mon_texte)

```

Le premier exemple montre comment créer une chaîne de caractères contenant le contenu que l'on souhaite écrire dans le fichier. Ensuite, à l'aide de la fonction `File.write`, nous pouvons écrire cette chaîne de caractères dans un fichier spécifié. Dans cet exemple, le fichier sera nommé "mon_fichier.txt".

Un autre exemple peut être de lire un fichier existant et de modifier son contenu avant de l'écrire dans un nouveau fichier :

```Gleam
// Lire le contenu du fichier original
let contenu = File.read("mon_fichier_original.txt")

// Modifier le contenu
let nouveau_contenu = contenu ++ "\nCeci est un ajout au fichier original."

// Ecrire le nouveau contenu dans un nouveau fichier
let fichier_modifie = File.write("mon_fichier_modifie.txt", nouveau_contenu)

```
Vous pouvez également écrire des données structurées dans un fichier en utilisant la fonction `File.write_json`. Cela peut être utile pour stocker des informations telles que des listes, des dictionnaires ou des données de configuration dans un format facilement lisible par d'autres programmes.

```Gleam
let mon_dictionnaire = Dict.from_list([("nom", "Paul"), ("age", 25), ("ville", "Paris")])
let fichier_json = File.write_json("mon_fichier.json", mon_dictionnaire)
```

## Approfondissement
Ecrire un fichier texte peut sembler simple, mais il y a plusieurs options supplémentaires que vous pouvez utiliser pour personnaliser l'écriture de votre fichier. Par exemple, vous pouvez spécifier le mode d'écriture (`"truncate"`, `"append"`, ou `"create"`), définir les permissions du fichier, et même ajouter un en-tête au début de votre fichier.

De plus, en utilisant des bibliothèques tierces telles que `gleam-csv` ou `gleam-yaml`, vous pouvez écrire des fichiers au format CSV ou YAML respectivement.

## Voir aussi
- La documentation officielle de Gleam sur l'écriture de fichiers : https://gleam.run/modules/file#write
- La bibliothèque `gleam-csv` pour écrire des fichiers CSV : https://github.com/onyxalpha/gleam-csv
- La bibliothèque `gleam-yaml` pour écrire des fichiers YAML : https://github.com/pawlik/gleam-yaml