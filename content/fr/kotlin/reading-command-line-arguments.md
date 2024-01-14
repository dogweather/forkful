---
title:                "Kotlin: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/kotlin/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation des arguments de ligne de commande peut être une compétence utile pour les programmeurs Kotlin, car cela leur permet de fournir des entrées personnalisées à leur programme lors de son exécution. Cela peut être utile pour effectuer certaines opérations spécifiques ou pour effectuer des tests et des débogages.

## Comment faire

Pour lire les arguments de ligne de commande en Kotlin, nous pouvons utiliser la fonction `main()` et l'objet `args`. Voici un exemple de code:

```Kotlin
fun main(args: Array<String>) {
  // Connexion à une base de données en utilisant les arguments de ligne de commande
  val database = Database(args[0], args[1])
  // Affichage de l'entrée personnalisée pour le nom d'utilisateur et le mot de passe
  println("Nom d'utilisateur: ${args[0]}, Mot de passe: ${args[1]}")
}
```

Si nous exécutons ce programme avec les arguments `john` et `1234`, nous obtiendrons la sortie suivante:

```
Nom d'utilisateur: john, Mot de passe: 1234
```

Nous pouvons également utiliser une boucle `for` pour parcourir tous les arguments de ligne de commande si nous ne connaissons pas à l'avance leur nombre. Par exemple:

```Kotlin
fun main(args: Array<String>) {
  // Affichage de chaque argument de ligne de commande
  for (arg in args) {
    println(arg)
  }
}
```

Si nous exécutons ce programme avec les arguments `1 2 3`, nous obtiendrons la sortie suivante:

```
1
2
3
```

## Plongée profonde

L'objet `args` est de type `Array<String>`, ce qui signifie que nous pouvons également utiliser des méthodes telles que `size` pour obtenir le nombre total d'arguments, ou `contains` pour vérifier si un argument spécifique a été fourni. Voici un exemple:

```Kotlin
fun main(args: Array<String>) {
  // Vérification si l'argument 'username' a été fourni
  if (args.contains("username")) {
    println("L'argument 'username' a été fourni")
  }
  // Affichage du nombre total d'arguments
  println("Nombre total d'arguments: ${args.size}")
}
```

Si nous exécutons ce programme avec les arguments `username=john`, nous obtiendrons la sortie suivante:

```
L'argument 'username' a été fourni
Nombre total d'arguments: 1
```

## Voir aussi

- [Documentation officielle Kotlin sur les arguments de ligne de commande](https://kotlinlang.org/docs/reference/command-line.html)
- [Tutoriel sur les arguments de ligne de commande en Kotlin](https://www.baeldung.com/kotlin/command-line-arguments)
- [Exemple de projet GitHub montrant l'utilisation des arguments de ligne de commande en Kotlin](https://github.com/Eliah-Senpai/Kotlin-Command-Line-Arguments-Example)