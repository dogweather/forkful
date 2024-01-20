---
title:                "Vérifier si un répertoire existe"
date:                  2024-01-20T14:56:26.408138-07:00
html_title:           "Go: Vérifier si un répertoire existe"
simple_title:         "Vérifier si un répertoire existe"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?

Vérifier l'existence d'un dossier, c'est s'assurer qu’un chemin spécifique est bien occupé par un dossier et non un fichier ou un néant. Les programmeurs le font pour éviter des erreurs lorsqu'ils accèdent ou manipulent des fichiers.

## Comment faire :

```gleam
import gleam/io
import gleam/result

pub fn check_directory_exists(path: String) -> result.Result(Nil, String) {
  match io.is_dir(path) {
    True ->
      result.Ok(Nil)
    False ->
      result.Err("Le dossier n'existe pas.")
  }
}

// Utilisation de la fonction
fn main() {
  let path = "chemin/vers/mon/dossier"
  let check = check_directory_exists(path)

  case check {
    Ok(_) -> io.print("Le dossier existe.")
    Err(err) -> io.print(err)
  }
}
```

Sortie possible :
```
Le dossier existe.
```
Ou, si le dossier n'existe pas :
```
Le dossier n'existe pas.
```

## Plongée Profonde

Historiquement, vérifier l'existence d'un dossier est une opération de base en programmation pour éviter des erreurs de lecture/écriture. En Gleam, qui compile vers Erlang, cette opération est aussi importante que dans les autres langues du BEAM comme Elixir ou Erlang lui-même.

Il existe des alternatives à `io.is_dir`, comme utiliser des appels système spécifiques ou des bibliothèques tierces, mais Gleam cherche à offrir une API standard simple et sûre, éliminant la nécessité de recourir à des solutions externes.

Au niveau de l'implémentation, `io.is_dir` fait appel aux fonctionnalités du système d'exploitation pour vérifier les métadonnées du chemin spécifié, ce qui rend l'opération généralement rapide et fiable.

## À Voir Aussi

- Documentation officielle de Gleam : [https://gleam.run](https://gleam.run)
- Forum de Gleam pour poser des questions ou partager des expériences : [https://github.com/gleam-lang/gleam/discussions](https://github.com/gleam-lang/gleam/discussions)