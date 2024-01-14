---
title:                "Elixir: Écriture vers l'erreur standard"
programming_language: "Elixir"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi écrire vers la sortie d'erreur?

Écrire vers la sortie d'erreur est utile lors de la détection et de la gestion des erreurs dans un programme Elixir. Cela peut permettre de mieux comprendre l'origine et la nature des erreurs, et faciliter la résolution de problèmes.

## Comment écrire vers la sortie d'erreur

Pour écrire vers la sortie d'erreur en Elixir, utilisez la fonction `IO.write` ou `IO.puts` en spécifiant `:stderr` comme deuxième argument. Par exemple :

```` elixir
IO.write(:stderr, "Erreur: Le nombre entré doit être positif")
````
Dans cet exemple, le message d'erreur sera écrit vers la sortie d'erreur.

Il est également possible d'écrire vers la sortie d'erreur avec des libellés de couleur en utilisant le module `IO.ANSI` et la fonction `IO.ANSI.format` :

```` elixir
IO.write(:stderr, IO.ANSI.format([:red, :bold], "Erreur critique"))
````

Cela affichera "Erreur critique" en rouge et en gras dans la sortie d'erreur.

## Plongez dans la sortie d'erreur

La sortie d'erreur en Elixir est gérée par le système d'exploitation, et peut donc varier selon la plateforme utilisée. Sur les systèmes basés sur Unix, tels que Linux ou macOS, `:stderr` est associé à un processus appelé "standard error file descriptor". Il est également possible d'utiliser `:stderr` avec des superviseurs et des processus de supervision en Elixir, pour gérer les erreurs de manière plus robuste.

Il est important de noter que lorsque l'exécution d'un programme génère des erreurs, elles ne seront pas toujours affichées automatiquement dans la sortie d'erreur. Il peut être nécessaire d'utiliser des outils tels que `logger` ou `sentry` pour capturer et gérer ces erreurs de manière plus efficace.

## Voir aussi

- [Documentation sur la sortie d'erreur en Elixir](https://hexdocs.pm/elixir/IO.html#write/2)
- [Guide sur la gestion des erreurs en Elixir](https://elixir-lang.org/getting-started/try-catch-and-rescue.html)
- [Article sur les meilleures pratiques de gestion des erreurs en Elixir](https://blog.appsignal.com/2018/08/14/elixir-alchemy-best-practices-for-error-handling-and-rescue.html)