---
title:                "Gleam: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Pourquoi

Lorsque vous programmez, il peut être utile de vérifier si un répertoire existe avant de continuer avec votre code. Cela peut vous éviter des erreurs et des problèmes avec vos fichiers. Dans cet article, nous allons expliquer comment faire cette vérification en utilisant le langage de programmation Gleam.

## Comment faire

Pour vérifier si un répertoire existe en Gleam, vous pouvez utiliser la fonction `os.file_exists`. Elle prend en paramètre le chemin complet du répertoire que vous souhaitez vérifier et retourne un booléen indiquant s'il existe ou non.

```Gleam
// Vérifie si le répertoire "/mon/dossier/exemple" existe
let existe = os.file_exists("/mon/dossier/exemple")

if existe {
  // Si le répertoire existe, effectuez une action
  ...
} else {
  // Si le répertoire n'existe pas, effectuez une autre action
  ...
}
```

Il est également possible d'utiliser la fonction `os.dir_exists` si vous souhaitez spécifiquement vérifier l'existence d'un répertoire et non d'un fichier.

```Gleam
// Vérifie si le répertoire "/mon/dossier/exemple" existe
let existe = os.dir_exists("/mon/dossier/exemple")

if existe {
  // Si le répertoire existe, effectuez une action
  ...
} else {
  // Si le répertoire n'existe pas, effectuez une autre action
  ...
}
```

## Plongée profonde

La fonction `os.file_exists` et `os.dir_exists` utilisent le système de fichiers sous-jacent de votre ordinateur pour vérifier l'existence du répertoire. Elle renvoie un booléen en fonction de la réponse du système de fichiers. Si vous rencontrez des problèmes avec ces fonctions, il est possible que le problème soit avec le système de fichiers plutôt qu'avec votre code.

## Voir aussi

- Documentation officielle de Gleam sur les fonctions `os.file_exists` et `os.dir_exists`: https://gleam.run/documentation/stdlib/os.html#function.file_exists
- Exemples de projets utilisant la vérification d'existence de répertoires en Gleam: https://github.com/search?q=language%3Agleam+os.file_exists&type=Code