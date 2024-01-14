---
title:    "Elixir: Trouver la longueur d'une chaîne de caractères"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Trouver la longueur d'une chaîne de caractères peut sembler être une tâche simple, mais cela peut en réalité être très utile dans la programmation. Cela vous permet de manipuler et de traiter efficacement des données textuelles, ce qui est un élément clé dans la plupart des programmes informatiques.

## Comment faire

Pour trouver la longueur d'une chaîne de caractères en Elixir, nous pouvons utiliser la fonction `String.length()`. Voici un exemple de code pour trouver la longueur d'une chaîne de caractères en utilisant cette fonction:

```Elixir
string = "Bonjour le monde!"
length = String.length(string)
IO.puts(length) #output: 18
```

Nous pouvons également combiner la fonction `String.length()` avec la méthode de liste `hd()` pour trouver la longueur de mots individuels dans une phrase. Voici un exemple:

```Elixir
string = "Le petit chat noir"
words = String.split(string, " ")
IO.puts("Le mot \"#{hd(words)}\" a une longueur de #{String.length(hd(words))}.") #output: Le mot "Le" a une longueur de 2.
```

Dans ces exemples, nous avons utilisé la méthode `IO.puts()` pour afficher le résultat à l'écran, mais vous pouvez également l'utiliser pour manipuler les données selon vos besoins.

## Plongée en profondeur

Il y a quelques choses à garder à l'esprit lors de l'utilisation de la fonction `String.length()` en Elixir. Tout d'abord, cette fonction renvoie le nombre de caractères unicode dans la chaîne de caractères et non le nombre de lettres ou de symboles. Une autre chose à noter est que, lors du traitement de chaînes de caractères contenant des caractères unicode comme les emojis, la longueur réelle peut être différente de la longueur affichée.

## Voir aussi

- La documentation officielle sur `String.length()` : https://hexdocs.pm/elixir/String.html#length/1
- Un tutoriel sur la manipulation de chaînes de caractères en Elixir : https://www.youtube.com/watch?v=ayla9-PjcXQ