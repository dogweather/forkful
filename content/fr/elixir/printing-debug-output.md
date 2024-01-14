---
title:    "Elixir: Impression de sortie de débogage"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

L'impression de sortie de débogage est une partie essentielle de la programmation Elixir. Cela vous permet de suivre le flux de votre code et de comprendre où les erreurs se produisent. Sans cela, il peut être difficile de résoudre les problèmes dans votre application.

## Comment faire

Pour imprimer une sortie de débogage en Elixir, vous pouvez utiliser la fonction `IO.inspect/2`. Cette fonction prend en paramètre la valeur que vous souhaitez inspecter et affiche le résultat dans la console. Par exemple :

```elixir
age = 25
IO.inspect(age)
```

Cela affichera `25` dans la console. Vous pouvez également utiliser `IO.inspect/2` pour inspecter des variables dans une liste ou un tuple :

```elixir
names = ["Alice", "Bob", "Jane"]
IO.inspect(names, label: "List of Names")
```

Cela affichera :

```
List of Names: ["Alice", "Bob", "Jane"]
```

Vous pouvez également utiliser `IO.inspect/2` pour inspecter des expressions :

```elixir
sum = 2 + 3
IO.inspect(sum, label: "Result")
```

Cela affichera :

```
Result: 5
```

## Plongée en profondeur

Il existe plusieurs options que vous pouvez utiliser avec `IO.inspect/2` pour personnaliser votre sortie de débogage. Par exemple, vous pouvez utiliser le paramètre `pretty: true` pour formater votre sortie de manière plus agréable à lire. Vous pouvez également utiliser `color: [syntax: :dark]` pour colorer votre sortie dans la console.

De plus, vous pouvez utiliser `IO.inspect/2` à l'intérieur de votre code pour suivre le flux de vos opérations et comprendre comment les données sont transformées. Cela peut être particulièrement utile lors de la résolution de bugs dans des fonctions complexes.

## Voir aussi

- [Documentation IO.inspect/2](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Article sur l'utilisation de IO.inspect en Elixir](https://www.learnelixir.com/blog/elixir-tips-io-inspect/)
- [Vidéo sur les techniques de débogage en Elixir](https://www.youtube.com/watch?v=MyHcKYkAh98)