---
title:                "Création d'un fichier temporaire"
html_title:           "Kotlin: Création d'un fichier temporaire"
simple_title:         "Création d'un fichier temporaire"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Créer un fichier temporaire consiste à générer un fichier à usage éphémère dans le système. Les programmeurs le font pour des raisons variées, notamment: stocker des données intermédiaires, manipuler ces données de manière isolée, ou tester le comportement de leur programme en situation réelle.

## Comment faire:
En Gleam, vous pouvez créer des fichiers temporaires en utilisant les fonctions fournies par le module `gleam/filesystem/tempfile`. Jetons un coup d'oeil à un exemple basique:

```gleam
import gleam/filesystem/tempfile

fn main() {
  let tmpfile = tempfile.new(".txt")
  //...
  tmpfile.delete() // N'oubliez pas de supprimer le fichier temporaire une fois son usage terminé!
}
```

Dans cet exemple, nous créons d'abord un nouveau fichier temporaire avec l'extension `.txt`, puis nous le supprimons une fois que nous avons terminé d'utiliser ses données.

## Plongée en profondeur
La création de fichiers temporaires est une pratique qui remonte aux débuts de l'informatique, lorsque les ressources de stockage étaient rares et coûteuses. Aujourd'hui, elle reste une pratique courante pour de nombreuses raisons, comme mentionné plus haut.

Une alternative à la création de fichiers temporaires serait d'utiliser la mémoire vive (RAM), mais cela peut être coûteux en termes de performances et d'espace, surtout pour de gros volumes de données.

Avec Gleam, la création d'un fichier temporaire est réalisée en interne en utilisant les APIs fournies par le système d'exploitation sous-jacent. C'est pourquoi l'extension du fichier peut être spécifiée lors de la création, permettant ainsi le stockage de différents types de données.

## Voir Aussi
Pour plus d'informations sur la création de fichiers temporaires en Gleam, consultez la [documentation officielle du module tempfile](https://hexdocs.pm/gleam_stdlib/gleam/filesystem/tempfile). Pour des tutorials plus détaillés sur la programmation en Gleam, consultez [ce guide](https://gleam.run/tour/introduction/). Enfin, pour une compréhension plus approfondie des fichiers temporaires et de leurs usages, le livre ["Operating Systems: Three Easy Pieces"](http://pages.cs.wisc.edu/~remzi/OSTEP/) offre une excellente vue d'ensemble.