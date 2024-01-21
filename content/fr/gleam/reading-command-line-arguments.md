---
title:                "Lecture des arguments de ligne de commande"
date:                  2024-01-20T17:56:00.036931-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?
Lire les arguments de la ligne de commande, c'est récupérer les infos fournies par l'utilisateur au lancement d'un programme. Les développeurs l'utilisent pour personnaliser l'exécution sans modifier le code.

## How to:
```Gleam
pub fn main(args: List(String)) {
  let arg1 = args.nth(1) // On récupère le deuxième élément car le premier est toujours le chemin du programme
  let message = case arg1 {
    Some(value) -> value
    None -> "Hey, tu as oublié de passer un argument!"
  }
  io.println(message)
}
```
**Sortie si argument fourni :**
```
$ gleam run votre_programme "Salut le monde!"
Salut le monde!
```
**Sortie si aucun argument :**
```
$ gleam run votre_programme
Hey, tu as oublié de passer un argument!
```

## Deep Dive
Historiquement, l'accès aux arguments de la ligne de commande est essentiel, permettant aux programmes d'être flexibles. Alternativement, on peut utiliser des fichiers de configuration ou des variables d'environnement, mais les arguments sont idéals pour des instructions ponctuelles et rapides. En Gleam, on accède aux arguments par la fonction `main` qui les reçoit sous forme de `List(String)`, pratique pour les traiter avec les puissantes fonctions de liste de Gleam.

## See Also
- Documentation officielle sur les Strings de Gleam : [Gleam Strings](https://gleam.run/book/tour/strings.html)
- Un tour de Gleam : [A Tour of Gleam](https://gleam.run/book/tour/index.html)