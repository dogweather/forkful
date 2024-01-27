---
title:                "Écriture d'un fichier texte"
date:                  2024-01-19
html_title:           "Arduino: Écriture d'un fichier texte"
simple_title:         "Écriture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Écrire dans un fichier texte permet de sauvegarder des données de manière durable. Les programmeurs le font pour conserver des configurations, des logs, ou partager des informations entre différents systèmes.

## Comment faire :

```gleam
import gleam/io
import gleam/result

// Écrire dans un fichier texte (hello.txt) avec le contenu "Salut, Gleam!".
pub fn ecrire_fichier() {
  let resultat = io.write_file("hello.txt", "Salut, Gleam!")
  case resultat {
    Ok(_) -> io.println("Fichier écrit avec succès!")
    Error(err) -> io.println("Erreur lors de l'écriture du fichier: " ++ err)
  }
} 
```

Sortie attendue :

```
Fichier écrit avec succès!
```

## Exploration :

Historiquement, l’écriture dans des fichiers est un moyen basique mais essentiel de persistance des données. En Gleam, `io.write_file` est une approche simplifiée parmi d'autres méthodes plus complexes comme les flux de données ou les bases de données. Les détails d'implémentation reposent sur les systèmes d'entrées/sorties fournies par Erlang VM, une plateforme réputée pour sa robustesse.

## Voir Aussi :

- Guide pratique Erlang pour les E/S de fichiers : [http://erlang.org/doc/man/file.html](http://erlang.org/doc/man/file.html)
