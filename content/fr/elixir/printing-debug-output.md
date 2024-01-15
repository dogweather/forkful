---
title:                "Sortie de débogage par impression"
html_title:           "Elixir: Sortie de débogage par impression"
simple_title:         "Sortie de débogage par impression"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi imprimer des sorties de débogage ? Eh bien, cela peut être utile lorsque vous rencontrez des problèmes avec votre code et que vous avez besoin de plus d'informations sur ce qui se passe. Cela peut également être utile pour comprendre comment votre code fonctionne et vérifier que les valeurs des variables sont ce que vous attendez.

## Comment faire

Impressionner des sorties de débogage en Elixir est très simple. Il vous suffit d'utiliser la fonction `IO.inspect/2` en lui passant la valeur que vous souhaitez inspecter. Voici un exemple :

```Elixir
liste = [1, 2, 3]
IO.inspect(liste)
```

Cela affichera `[1, 2, 3]` dans la console, ce qui vous permettra de vérifier les valeurs dans votre liste.

Vous pouvez également utiliser `IO.inspect/2` pour afficher plusieurs valeurs en une seule fois :

```Elixir
a = 1
b = 2
c = 3
IO.inspect(a, b, c)
```

Cela affichera `1 2 3` dans la console. Vous pouvez également utiliser `IO.inspect/2` pour afficher les valeurs de variables dans une chaîne :

```Elixir
a = 1
b = 2
IO.inspect("La valeur de a est #{a} et la valeur de b est #{b}")
```

Cela affichera `"La valeur de a est 1 et la valeur de b est 2"` dans la console.

## Plongée en profondeur

Maintenant que vous savez comment utiliser `IO.inspect/2`, il est important de noter qu'il est recommandé de l'utiliser à des fins de débogage uniquement. En effet, l'utilisation excessive de cette fonction peut ralentir l'exécution de votre code.

De plus, vous pouvez également personnaliser la façon dont `IO.inspect/2` affiche les valeurs en lui passant une liste d'options en deuxième paramètre. Par exemple, vous pouvez spécifier `pretty: true` pour afficher la valeur sous une forme plus lisible, ou `limit: 10` pour limiter le nombre d'éléments affichés pour les structures de données complexes.

Vous pouvez consulter la documentation officielle pour plus d'informations sur les différentes options disponibles pour `IO.inspect/2`, ainsi que d'autres fonctions utiles pour le débogage en Elixir.

## Voir aussi

- Documentation officielle Elixir sur `IO.inspect/2`: https://hexdocs.pm/elixir/IO.html#inspect/2
- Article sur le débogage en Elixir: https://dev.to/ohmme/elixir-debugging-with-cond-and-binding-5373