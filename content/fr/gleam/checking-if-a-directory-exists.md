---
title:                "Vérification de l'existence d'un répertoire"
html_title:           "Gleam: Vérification de l'existence d'un répertoire"
simple_title:         "Vérification de l'existence d'un répertoire"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

Que & Pourquoi?
Vérifier si un répertoire existe est une tâche courante en programmation qui consiste à s'assurer qu'un répertoire spécifié existe avant d'effectuer certaines opérations dessus. Cela permet d'éviter les erreurs et les plantages lors de l'exécution du programme.

Comment:
Voici un exemple de code en Gleam pour vérifier si un répertoire existe, en utilisant la fonction "os.exits?" et en imprimant le résultat :

```Gleam
let dir = "/chemin/vers/le/repertoire"
let exists = os.exists?(dir)
io.println("Le répertoire existe-t-il? ${exists}")
```

Sortie:
```
Le répertoire existe-t-il? true
```

Vous pouvez également utiliser une clause "if" pour effectuer une action spécifique si le répertoire existe ou non :

```Gleam
let dir = "/chemin/vers/le/repertoire"
let exists = os.exists?(dir)
if exists {
  // Actions à effectuer si le répertoire existe
} else {
  // Actions à effectuer si le répertoire n'existe pas
}
```

Deep Dive:
Historiquement, la vérification de l'existence d'un répertoire se faisait en utilisant une fonction native du système d'exploitation tel que "dir" sur Windows ou "ls" sur Unix. Cependant, cette méthode peut être moins portable et dépendante du système d'exploitation.

Il existe également d'autres alternatives en Gleam, telles que l'utilisation de bibliothèques externes telles que "fs" ou l'utilisation de la fonction "walk" pour parcourir les fichiers et répertoires.

See Also:
- Documentation officielle de Gleam sur la fonction os.exists?: https://gleam.run/book/standard-library.html#os.exists
- Guide de référence de Gleam pour la manipulation des fichiers: https://gleam.run/book/files.html
- Bibliothèque externe "fs" pour la manipulation de fichiers et répertoires en Gleam: https://github.com/gleam-lang/fs