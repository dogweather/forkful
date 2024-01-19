---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Lire les arguments de la ligne de commande d'un programme, c'est pouvoir passer des informations au programme lors de son exécution. Les programmeurs le font pour manipuler le comportement du programme et lui faire effectuer des actions spécifiques.

## Comment faire :
Prenons un exemple avec du code Gleam pour lire les arguments de la ligne de commande:

```Gleam
import gleam/list
import gleam/io

fn main(args: List(String)) {
  args
  |> list.reverse
  |> list.map(|arg| io.println(arg))
  |> result.unwrap
}
```

Si vous exécutez cette commande : `gleam run my_program arg1 arg2 arg3`, cela donnera l'affichage suivant :

```
arg3
arg2
arg1
```
## Plongée en profondeur :
Historiquement, la lecture des arguments de la ligne de commande a été un moyen pour les systèmes UNIX de faire passer des informations aux programmes. Aujourd'hui, même les systèmes d'exploitation modernes comme Windows prennent en charge cette fonctionnalité.

Les alternatives à la lecture des arguments de la ligne de commande comprennent le passage des données via des fichiers de configuration ou des variables d'environnement. Cependant, ces méthodes peuvent être plus lourdes à gérer.

Au niveau de l'implémentation dans Gleam, la fonction main reçoit une liste de chaînes de caractères. Chaque élément de la liste est un argument de la ligne de commande. 

## Voir aussi :
Pour aller plus loin, voici quelques ressources sur la gestion des arguments de ligne de commande dans différents contextes :

- Documentation de Gleam sur `list` et `io` : https://hexdocs.pm/gleam/gleam/list.html , https://hexdocs.pm/gleam/gleam/io.html
- Pour en savoir plus sur les arguments de ligne de commande en général : https://fr.wikipedia.org/wiki/Argument_(informatique)
- Un cours sur l'utilisation des arguments de ligne de commande en Python pour comparaison : https://docs.python.org/fr/3/tutorial/stdlib.html#command-line-arguments