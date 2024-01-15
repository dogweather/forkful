---
title:                "Vérifier si un répertoire existe"
html_title:           "Gleam: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Pourquoi

Quelle est l'utilisation de vérifier l'existence d'un répertoire ? En informatique, il est parfois nécessaire de s'assurer que certains fichiers ou dossiers existent avant de procéder à d'autres actions. Cela peut être utile lors de la manipulation de données ou lors de la création de nouveaux fichiers.

## Comment faire

Pour vérifier l'existence d'un répertoire en utilisant Gleam, nous pouvons utiliser la fonction `fs.exists` en lui passant en argument le chemin du répertoire que nous souhaitons vérifier. Voici un exemple de code :

```Gleam
import gleam/fs

let directory = "/chemin/vers/repertoire"

if fs.exists(directory) {
    // Le répertoire existe, nous pouvons continuer notre processus
    // ...
} else {
    // Le répertoire n'existe pas, nous pouvons gérer cela comme nous le souhaitons
    // ...
}
```

Dans cet exemple, nous importons d'abord le module `gleam/fs` qui contient la fonction `exists`. Ensuite, nous définissons le chemin du répertoire que nous voulons vérifier. Dans la condition `if`, nous utilisons la fonction `exists` en lui passant le chemin du répertoire en argument. Si la fonction renvoie `true`, cela signifie que le répertoire existe et nous pouvons continuer notre processus. Sinon, c'est que le répertoire n'existe pas et nous pouvons gérer cela comme bon nous semble.

## Plongée en profondeur

En réalité, la fonction `fs.exists` ne vérifie pas uniquement l'existence d'un répertoire, mais aussi de fichiers ou de liens symboliques. Elle renvoie une valeur `Ok`, contenant `true` si l'élément existe et `false` sinon, ou une valeur `Err` en cas d'erreur. Nous pouvons également utiliser la fonction `fs.is_dir` pour vérifier si un élément est un répertoire spécifique.

# Voir aussi

- [Documentation officielle de Gleam sur les fonctions de gestion des fichiers](https://gleam.run/modules/fs.html)
- [Tutoriel Gleam pour débutants](https://gleam.run/docs/tour/introduction.html)