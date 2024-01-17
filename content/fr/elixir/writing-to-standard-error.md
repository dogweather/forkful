---
title:                "Écrire vers l'erreur standard"
html_title:           "Elixir: Écrire vers l'erreur standard"
simple_title:         "Écrire vers l'erreur standard"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi & Comment écrire sur la sortie d’erreur standard en Elixir

## Quoi & Pourquoi?
Écrire sur la sortie d’erreur standard est une pratique courante pour les programmeurs en Elixir. Cela consiste à afficher des messages d'erreur de manière explicite et à les diriger vers la sortie d’erreur standard plutôt que vers la sortie standard. Cela peut aider les développeurs à identifier et à corriger rapidement les erreurs dans leur code.

## Comment faire:
Pour écrire sur la sortie d’erreur standard en Elixir, utilisez la fonction ```IO.puts/1``` en passant en paramètre le message d'erreur. Par exemple:
```Elixir
IO.puts("Une erreur est survenue!")
```
Cela affichera le message dans la sortie d’erreur standard, généralement en rouge pour le différencier de la sortie standard.

Vous pouvez également utiliser la fonction ```IO.inspect/1``` pour afficher un message d'erreur avec d'autres informations utiles telles que le contenu d'une variable. Par exemple:
```Elixir
a = 5
IO.inspect("La valeur de a est #{a}")
```

## Plongée en profondeur:
Bien que l'utilisation de la sortie d’erreur standard soit courante en Elixir, il est important de savoir qu’il existe d'autres alternatives telles que la journalisation ou la gestion d'erreurs avec les mots-clés ```raise``` et ```try...rescue```. Ces alternatives peuvent être plus adaptées en fonction du contexte et des objectifs du programme.

En ce qui concerne l'implémentation, la sortie d’erreur standard est gérée par le système d'exploitation plutôt que par Elixir lui-même. Cela signifie que le fonctionnement peut varier en fonction du système d'exploitation utilisé.

## À voir:
Pour en savoir plus sur l'utilisation de la sortie d’erreur standard en Elixir, consultez la documentation officielle sur [IO.puts/1](https://hexdocs.pm/elixir/IO.html#puts/1) et [IO.inspect/1](https://hexdocs.pm/elixir/IO.html#inspect/1).

Vous pouvez également trouver des informations utiles sur les alternatives à la sortie d’erreur standard dans cet [article](https://elixirschool.com/en/lessons/basics/error-handling/) d'Elixir School.