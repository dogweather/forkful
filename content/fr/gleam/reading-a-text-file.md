---
title:                "Lecture d'un fichier texte"
date:                  2024-01-20T17:54:24.372490-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (Quoi et pourquoi?)
Lire un fichier texte, c'est récupérer son contenu pour l'utiliser dans notre programme. Les programmeurs le font pour traiter des données, configurer des systèmes ou alimenter des applications avec des informations.

## How to: (Comment faire:)
```gleam
import gleam/io

pub fn main() {
  // Try lire un fichier "example.txt" et traiter l'erreur si nécessaire
  let result = io.read_file("example.txt")
  case result {
    Ok(content) -> io.println(content)
    Error(error) -> io.println("Oops, quelque chose s'est mal passé: " ++ error)
  }
}
```
Sample output (Sortie d'exemple) :
```
Bonjour, voici le contenu du fichier texte !
```

## Deep Dive (Plongée en profondeur)
Historiquement, lire des fichiers textes est un besoin de base en informatique, avec des utilisations allant du simple stockage de données au contrôle de logiciels complexes. En Gleam, comme dans beaucoup d'autres langages fonctionnels, lire un fichier se fait de manière explicite pour gérer les erreurs potentielles de façon propre.

Alternatives, vous avez par exemple `stream_file` pour lire de gros fichiers sans trop utiliser de mémoire. En termes de détails d'implémentation, on gère les erreurs grâce au pattern matching sur `Result`, qui sépare clairement succès (Ok) et erreur (Error).

## See Also (Voir aussi)
- Documentation Gleam sur IO : https://gleam.run/book/tour/io.html
- Tutoriel sur le Pattern Matching : https://gleam.run/book/tour/pattern-matching.html
- Guide Gleam pour les erreurs : https://gleam.run/book/tour/errors.html
