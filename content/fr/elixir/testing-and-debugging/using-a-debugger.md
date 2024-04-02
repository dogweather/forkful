---
date: 2024-01-26 03:48:20.046588-07:00
description: "Elixir est livr\xE9 avec un d\xE9bogueur graphique int\xE9gr\xE9 appel\xE9\
  \ `:debugger`. Pour l'utiliser, vous devrez le d\xE9marrer et vous attacher \xE0\
  \ votre processus en\u2026"
lastmod: '2024-03-13T22:44:57.332030-06:00'
model: gpt-4-0125-preview
summary: "Elixir est livr\xE9 avec un d\xE9bogueur graphique int\xE9gr\xE9 appel\xE9\
  \ `:debugger`. Pour l'utiliser, vous devrez le d\xE9marrer et vous attacher \xE0\
  \ votre processus en\u2026"
title: "Utilisation d'un d\xE9bogueur"
weight: 35
---

## Comment faire :
Elixir est livré avec un débogueur graphique intégré appelé `:debugger`. Pour l'utiliser, vous devrez le démarrer et vous attacher à votre processus en cours d'exécution.

Tout d'abord, assurez-vous d'avoir démarré `:debugger` dans une session `iex` :
```elixir
iex> :debugger.start()
{:ok, #PID<0.108.0>}
```

Maintenant, interprétez le module de code que vous souhaitez déboguer :
```elixir
iex> :int.ni(MyApp.MyModule)
{:module, MyApp.MyModule}
```

Vous pouvez placer un point d'arrêt :
```elixir
iex> :int.break(MyApp.MyModule, numero_de_ligne)
:ok
```

Et ensuite, exécutez votre fonction pour atteindre le point d'arrêt et parcourir votre code :
```elixir
iex> MyApp.MyModule.ma_fonction(arg1, arg2)
# Le débogueur mettra en pause l'exécution à la ligne avec le point d'arrêt
```

## Plongée Profonde
Avant le `:debugger` d'Elixir, Erlang fournissait le débogueur qu'Elixir utilise ; il est robuste et excellent pour gérer les processus concurrents, un point fort de la VM Erlang (BEAM). Contrairement à certains autres débogueurs, le `:debugger` ne permet pas la modification des variables à la volée, en raison de la nature immuable des données dans Elixir. Quant aux alternatives, vous avez `IEx.pry` qui vous permet de mettre en pause l'exécution et de sauter dans un REPL à n'importe quel point de votre code, ce qui peut être très pratique.

Alors que `:debugger` est bien pour une interface graphique, certains pourraient préférer l'outil intégré `:observer` qui offre également l'inspection des processus et les métriques du système, bien qu'il ne soit pas spécifiquement ciblé sur le parcours du code. La communauté d'Elixir contribue également avec des outils comme `visualixir` et `rexbug`, élargissant l'écosystème des outils de débogage au-delà des options par défaut.

## Voir Aussi
- Guide de démarrage officiel d'Elixir sur le débogage : https://elixir-lang.org/getting-started/debugging.html
- Documentation de `:debugger` d'Erlang : http://erlang.org/doc/apps/debugger/debugger_chapter.html
- Discussions sur le forum d'Elixir sur les techniques de débogage : https://elixirforum.com/c/elixir-questions/elixir-questions-questions-help/15
