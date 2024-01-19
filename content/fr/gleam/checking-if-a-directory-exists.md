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

## Quoi et Pourquoi?
Vérifier si un répertoire existe est une opération qui consiste à confirmer l'existence ou la non-existence d'un répertoire spécifique. Les programmeurs le font pour éviter les erreurs de chemin d'accès lors de l'exécution de code qui nécessite l'accès à des fichiers ou des répertoires spécifiques.

## Comment faire:
Dans Gleam, vous pouvez utiliser la fonction `does_exist` du module `file` pour vérifier si un répertoire existe. Voici un exemple:

```gleam
import gleam/file

pub fn directory_exists(directory: String) -> Result(Bool, Nil) {
  file.does_exist(directory)
}
```
Cette fonction renvoie `Ok(True)` si le répertoire existe et `Ok(False)` si ce n'est pas le cas.

## Plongée en profondeur
La nécessité de vérifier l'existence d'un répertoire remonte à l'époque où les erreurs de chemin d'accès pouvaient entraîner des problèmes graves dans le traitement des données. 

Dans Gleam, il y a des alternatives comme `is_file` et `is_dir` du module `file` qui vérifient respectivement si le chemin donné est un fichier ou un répertoire. 

La fonction `does_exist` utilise la fonction standard de Erlang qui est utilisée pour accéder au système de fichiers, en prenant le nom de répertoire comme argument et en renvoyant le résultat après avoir effectué la vérification.

## Voir également
- Documentation Gleam: https://gleam.run/book/tour/
- Gleam `file` module: https://hexdocs.pm/gleam_stdlib/gleam/file

Souvenez-vous, les outils sont là pour vous aider. Utilisez-les judicieusement pour créer des programmes robustes et sans erreurs!