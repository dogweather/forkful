---
title:                "Lecture des arguments en ligne de commande"
html_title:           "Gleam: Lecture des arguments en ligne de commande"
simple_title:         "Lecture des arguments en ligne de commande"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

## Comment faire

La lecture des arguments de ligne de commande peut sembler être une tâche simple et ennuyeuse, mais elle peut en fait être très utile dans de nombreux cas. Que vous souhaitiez personnaliser le comportement de votre programme en fonction de l'entrée de l'utilisateur ou que vous ayez besoin d'informations spécifiques pour le débogage, la lecture des arguments de ligne de commande peut être une compétence très pratique à avoir.

Pour lire les arguments de ligne de commande en utilisant Gleam, nous devons utiliser le module `std/args`. Ce module fournit des fonctions qui nous permettent de récupérer les arguments passés à notre programme, de les traiter et de les utiliser selon nos besoins.

Voici un exemple simple de lecture des arguments de ligne de commande en utilisant Gleam:

```
Gleam import std/args

fn main(_args) {
    Gleam let args = Args.parse(_args)
    Gleam let name = Args.flag("--name", "World", args)
    Gleam IO.print("Hello, ") ++ name ++ "!"
}
```

Dans cet exemple, nous importons d'abord le module `std/args`. Nous utilisons ensuite la fonction `Args.parse` pour convertir les arguments passés à notre programme en un type de données approprié. Ensuite, nous utilisons la fonction `Args.flag` pour récupérer la valeur de l'argument nommé `--name` si elle existe. Si l'argument n'est pas passé, nous utilisons la valeur par défaut "World". Enfin, nous utilisons la fonction `IO.print` pour afficher un message de salutation en utilisant la valeur de `name`.

Si nous exécutons notre programme en utilisant la commande suivante :

```
$ gleam run hello.gleam --name "John"
```

Nous obtiendrons la sortie suivante :

```
Hello, John!
```

Maintenant, que se passe-t-il si nous voulons passer plusieurs arguments de ligne de commande ? Gleam a également une fonction appelée `Args.non_flag_arguments` qui nous permet de récupérer tous les arguments qui ne sont pas des drapeaux (flags). Par exemple :

```
fn main(_args) {
    let args = Args.parse(_args)
    let name = Args.flag("--name", "World", args)
    let arguments = Args.non_flag_arguments(args)

    IO.print("Hello, ") ++ name ++ "!"
    Gleam IO.print("You passed the following arguments: ") ++ arguments
}
```

Si nous exécutons notre programme avec la commande suivante :

```
$ gleam run hello.gleam --name "John" "Gleam is awesome"
```

Nous obtiendrons cette sortie :

```
Hello, John!
You passed the following arguments: ["Gleam", "is", "awesome"]
```

## Plongée en profondeur

Maintenant que nous avons vu comment lire les arguments de ligne de commande en utilisant Gleam, il est important de noter que le module `std/args` offre d'autres fonctions pratiques pour la gestion des arguments. Par exemple, vous pouvez utiliser la fonction `Args.flag_with_default` pour définir une option avec une valeur par défaut, ou la fonction `Args.flag_with_callback` pour exécuter une fonction spécifique lorsque l'argument est passé.

Nous pouvons également utiliser des drapeaux de type booléen avec la fonction `Args.flag_with_default`, qui sera true par défaut si l'argument est passé et false sinon. Nous pouvons également utiliser des valeurs de type `string` pour les arguments avec la fonction `Args.flag_string`, ou des valeurs de type `int` avec la fonction `Args.flag_int`. Pour plus d'informations sur ces fonctions et leurs utilisations, consultez la documentation du module `std/args`.

## Voir aussi 

- Documentation sur les arguments de ligne de commande en Gleam : https://gleam.run/modules/std.args.html
- Tutoriel sur l'écriture de ligne de commande en Gleam : https://ellipses.io/learn/cli-app-gleam/