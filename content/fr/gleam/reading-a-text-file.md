---
title:                "Lecture d'un fichier texte"
html_title:           "Arduino: Lecture d'un fichier texte"
simple_title:         "Lecture d'un fichier texte"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Lire un fichier texte en programmation consiste à styphiser les lignes de code dans le fichier, une par une. Les programmeurs le font pour obtenir ou analyser des données, ou pour mettre en œuvre des tâches automatisées.

## Comment faire:

Avec Gleam, nous pouvons utiliser la bibliothèque `gleam/otp` pour lire des fichiers texte. Voici un exemple simple:

```Gleam
import gleam/otp.{File, Line}
import gleam/otp.file.{open, Mode}

let read_text_file = fn(path: String) {
  case open(path, Mode.Read) {
    Ok(file) ->
      loop(file, [])
    Error(err) ->
      Error(err)
  }
}

let rec loop = fn(file: File, lines: List(Line)) {
 case gleam/otp.file.read(file) {
    Ok(line) ->
      loop(file, [line | lines])
    Error(end_of_file) ->
      Ok(list:reverse(lines))
    Error(other_error) ->
      Error(other_error)
 }
}
```

Cela retournera une liste de toutes les lignes dans le fichier texte.

## Plongeon Profond:

Historiquement, la lecture de fichiers texte est l'une des premières tâches que les ordinateurs étaient capables de faire. Elle a toujours été au cœur de la programmation. En Gleam, la `gleam/otp` est une bibliothèque open-source qui implémente un ensemble de comportements OTP (Open Telecom Platform).

Il existe d'autres façons de lire des fichiers texte en Gleam, vous pouvez aussi utiliser la bibliothèque `gleam/io`.

En ce qui concerne la mise en œuvre, Gleam est une langue typée statiquement, donc aucune donnée de runtime n'est requise pour le typage. La lecture de fichiers est effectuée ligne par ligne, ce qui est efficace pour les grands fichiers car toute la donnée du fichier n'a pas besoin d'être chargée en mémoire à la fois.

## Voir Aussi:

- Pour plus d'informations sur la bibliothèque `gleam/otp`, consultez la documentation officielle: https://hexdocs.pm/gleam_otp/readme.html
- Pour en savoir plus sur la bibliothèque `gleam/io`, voici le lien: https://hexdocs.pm/gleam_io/readme.html
- Pour en savoir plus sur le fonctionnement de Gleam, voici un bon ressource: https://gleam.run/learning/textbook.html