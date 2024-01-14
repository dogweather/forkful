---
title:    "Elixir: Afficher la sortie de débogage"
keywords: ["Elixir"]
---

{{< edit_this_page >}}

## Pourquoi

La publication de données de débogage peut être un outil utile pour comprendre le fonctionnement de votre code et résoudre les problèmes lors du développement en Elixir. En imprimant des données à des points clés dans votre code, vous pouvez suivre les valeurs des variables et les étapes de votre programme pour détecter les erreurs plus facilement.

## Comment faire

Pour imprimer des données de débogage dans votre code Elixir, vous pouvez utiliser la fonction `IO.inspect()`. Cette fonction prend en argument une variable ou une expression et l'imprime à la console. Voici un exemple:

```Elixir
iex> IO.inspect("Bonjour le monde!")
"Bonjour le monde!"
:ok
```

Dans l'exemple ci-dessus, nous imprimons la chaîne de caractères "Bonjour le monde!" à l'aide de la fonction `IO.inspect()` et nous obtenons comme résultat la même chaîne de caractères suivie de `:ok`, qui est la valeur de retour de la fonction.

Vous pouvez également utiliser l'option `label` pour spécifier un label à votre sortie de débogage, ce qui peut être utile si vous imprimez plusieurs variables à différents endroits de votre code.

```Elixir
iex> IO.inspect([1,2,3], label: "Liste de nombres")
Liste de nombres: [1, 2, 3]
:ok
```

## Plongée en profondeur

L'utilisation de la fonction `IO.inspect()` est un moyen simple et efficace de publier des données de débogage dans votre code Elixir. Cependant, il y a quelques choses à garder à l'esprit lors de l'utilisation de cette fonction:

- Il est important de ne pas surcharger votre code avec trop de publications de débogage. Cela peut rendre votre code plus difficile à lire et à comprendre, et peut même ralentir votre programme.
- Vous pouvez également utiliser la fonction `IO.puts()` pour imprimer du texte brut à la console sans ajouter de `:ok` à la fin de la sortie.
- Enfin, gardez à l'esprit que les données de débogage sont souvent utiles pendant la phase de développement, mais elles peuvent être inutiles dans un environnement de production.

## Voir aussi

- [Documentation officielle Elixir pour `IO.inspect()`](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Exemples plus avancés de la fonction `IO.inspect()`](https://elixir-lang.org/getting-started/debugging.html#inspecting-data)
- [Article sur la publication de données de débogage en Elixir](https://inquisitivedeveloper.com/debugging-in-elixir-the-quirk-of-the-implicit-end/)