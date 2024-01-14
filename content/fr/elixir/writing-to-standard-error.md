---
title:    "Elixir: Écrire sur l'erreur standard"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

Écrire sur la sortie d'erreur standard peut sembler être une tâche banale pour certains, mais c'est en fait une pratique utile pour déboguer et trouver des erreurs dans votre code. Dans cet article, nous verrons pourquoi il est important d'écrire sur la sortie d'erreur standard en tant que développeur Elixir.

## Comment faire

L'écriture sur la sortie d'erreur standard en Elixir est assez simple. Il suffit d'utiliser la fonction `IO.puts/2` en passant en premier argument `:stderr` pour spécifier la sortie d'erreur standard. Voici un exemple de code qui écrit "Hello World" sur la sortie d'erreur standard :

```Elixir
IO.puts(:stderr, "Hello World")
```

Lorsque vous exécutez ce code, vous devriez obtenir un résultat comme ceci :

```Elixir
Hello World
```

Cela peut sembler contre-intuitif d'écrire sur la sortie d'erreur standard pour afficher des messages, mais en réalité, cela peut s'avérer très utile lors de la recherche de bugs dans votre code ou simplement pour afficher des informations importantes pendant l'exécution du programme.

## Exploration en profondeur

L'écriture sur la sortie d'erreur standard peut être encore plus utile si vous ajoutez des informations supplémentaires aux messages. Vous pouvez utiliser la fonction `Kernel.inspect/2` pour afficher des valeurs dans un format lisible par l'homme. Voici un exemple de code qui affiche le contenu d'une liste sur la sortie d'erreur standard :

```Elixir
IO.puts(:stderr, "La liste contient : #{inspect([1, 2, 3])}")
```

Lors de l'exécution de ce code, vous devriez voir quelque chose comme ceci :

```Elixir
La liste contient : [1, 2, 3]
```

Cela peut vous aider à trouver des erreurs dans votre code en affichant les valeurs de variables à des endroits stratégiques de votre programme.

## Voir aussi

- [Documentation officielle sur la sortie d'erreur standard en Elixir](https://hexdocs.pm/elixir/Kernel.html#puts/2)
- [Article sur la lecture et l'écriture en Elixir](https://medium.com/@ockhrors/reading-and-writing-in-elixir-a-simple-approach-6966a7b9d2b4)
- [Vidéo explicative sur l'écriture sur la sortie d'erreur standard en Elixir](https://www.youtube.com/watch?v=RWzXK5B7WDE)