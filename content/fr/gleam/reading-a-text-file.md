---
title:                "Lecture d'un fichier texte"
html_title:           "Gleam: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Pourquoi

Tu te demandes peut-être pourquoi tu devrais lire un fichier texte. Eh bien, mon ami, laisse-moi te dire que la lecture de fichiers texte est essentielle pour tout codeur qui souhaite manipuler des données. Cet article va te montrer comment le faire avec Gleam.

## Comment faire

Tout d'abord, tu devras ouvrir le fichier texte en utilisant le module `gleam/io` et la fonction `open_file`. Tu devras également spécifier le chemin du fichier que tu veux lire. Voici un exemple de code :

```gleam
import gleam/io

pub fn read_text_file() {
    let file = io.open_file("chemin/vers/mon_fichier.txt") // chemin du fichier à lire
    case file {
        Ok(file_handle) -> {
            // Code pour lire le fichier ici
        }
        Err(error) -> {
            // Code pour gérer l'erreur ici
        }
    }
}
```

Maintenant, passons à la lecture du fichier. Utilise la fonction `read_line` pour lire une ligne de texte à la fois. Voici un exemple :

```gleam
let line = file_handle.read_line()
```

Tu peux également utiliser la fonction `read_all` pour lire tout le contenu du fichier en une seule fois.

## Plongeons plus en profondeur

La fonction `open_file` peut prendre un deuxième argument en plus du chemin du fichier. Il s'agit du mode d'ouverture du fichier, qui peut être "read", "write" ou "append". Si tu as besoin d'écrire ou de modifier un fichier texte, tu peux utiliser le mode "write" pour le fichier à ouvrir. Voici un exemple :

```gleam
let file = io.open_file("chemin/vers/mon_fichier.txt", "write")
```

Enfin, n'oublie pas de fermer le fichier une fois que tu as terminé de le lire en utilisant la fonction `close`.

## Voir aussi

- [Documentation officielle de Gleam](https://gleam.run/documentation)
- [Lire et écrire des fichiers avec Gleam](https://dev.to/betamarc/lire-et-ecrire-des-fichiers-avec-gleam-1c2h) (en anglais)