---
title:                "Elixir: Afficher la sortie de débogage"
programming_language: "Elixir"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi
Nous avons tous été dans cette situation où quelque chose ne fonctionne pas dans notre code et nous ne pouvons pas comprendre pourquoi. La solution? Impression du débogage. Dans cet article, nous allons discuter de pourquoi il est important d'imprimer des sorties de débogage dans notre code Elixir.

## Comment faire
Impression du débogage dans Elixir est très simple. Utilisez la fonction `IO.inspect()`, qui prend en paramètre la valeur que vous voulez inspecter. Voici un exemple de code:

```Elixir
IO.inspect("Bonjour le monde")
```

Et voici la sortie que vous obtiendrez:

```Elixir
"Bonjour le monde"
```

Vous pouvez également imprimer plusieurs valeurs en les séparant par des virgules:

```Elixir
IO.inspect("Bonjour", "le monde")
```

La sortie sera:

```Elixir
"Bonjour"
"le monde"
```

## Plongée en profondeur
L'impression du débogage peut sembler simple, mais cela peut être d'une grande aide dans le processus de débogage de votre code. En imprimant les valeurs de variables à des points clés de votre code, vous pouvez voir comment ces valeurs changent et ainsi identifier où se situe le problème.

De plus, en utilisant `IO.inspect()` avec des options supplémentaires, vous pouvez modifier la façon dont les valeurs sont imprimées. Par exemple, en utilisant `IO.inspect(value, label: "valeur")`, vous pouvez spécifier un label qui sera imprimé avant la valeur. De même, en utilisant `IO.inspect(value, pretty: true)`, vous obtiendrez une impression plus propre et plus facile à lire.

## Voir aussi
- [Documentation d'Elixir sur IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)
- [Article sur l'impression du débogage en Elixir par Flatiron School](https://medium.com/@rachelisblue/debugging-in-elixir-with-io-inspect-goes-beyond-puts-8e98a5284502)
- [Vidéo explicative sur l'impression du débogage en Elixir par ElixirSips](https://elixirsips.com/episodes/102-digging-in-with-ioputs)