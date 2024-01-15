---
title:                "Écrire sur la sortie d'erreur standard"
html_title:           "Elixir: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

Écrire vers l'erreur standard (standard error) peut sembler être une tâche obscure et peu plaisante, mais c'est en fait un outil utile pour un programmeur Elixir. En comprenant comment écrire vers l'erreur standard, vous pouvez mieux gérer les erreurs de votre code et améliorer la qualité de votre programme.

## Comment faire

L'écriture vers l'erreur standard utilise une fonction appelée :error_logger.log/1. Cette fonction prend en paramètre un enregistrement d'erreur et le transmet à l'erreur standard.

```Elixir
:error_logger.log("Une erreur s'est produite!")
```

Lorsque vous utilisez cette fonction, vous verrez cet enregistrement apparaître dans votre terminal avec une étiquette `[error]` avant le message, indiquant qu'il a été écrit vers l'erreur standard.

```
[error] Une erreur s'est produite!
```

Vous pouvez également spécifier le niveau de gravité de l'erreur en utilisant un tuple comme deuxième argument de la fonction. Par exemple, si vous voulez une erreur de niveau avertissement (warning) plutôt que d'erreur, vous pouvez utiliser le code suivant :

````elixir
:error_logger.log("Une erreur de niveau avertissement s'est produite!", {:warning})
```

Le programme affichera alors `[warning] Une erreur de niveau avertissement s'est produite!` dans le terminal.

## Plongée en profondeur

En utilisant les fonctions :error_logger, vous pouvez également enregistrer des informations sur le processus et le module qui a provoqué l'erreur. Par exemple, le code suivant utilise :error_logger pour afficher une erreur avec des informations sur le module et le processus en cours :

```elixir
:error_logger.log(
  "Une erreur s'est produite dans le module #{__MODULE__} avec le processus no #{self()}.",
  {:error, %{module: __MODULE__, process: self()}}
)
```

Cela permet de déboguer plus facilement votre code en identifiant précisément où l'erreur s'est produite. De plus, vous pouvez également spécifier des détails supplémentaires tels que la fonction et la ligne de code en utilisant les options `:function` et `:line`.

```elixir
:error_logger.log(
  "Une erreur s'est produite dans la fonction #{__CALLER__.function} à la ligne #{__CALLER__.line}.",
  {:error, %{function: __CALLER__.function, line: __CALLER__.line}}
)
```

## Voir aussi

- Documentation officielle sur les fonctions :error_logger (https://hexdocs.pm/elixir/ErrorLogger.html)
- Un article détaillé sur les différents types de fonctions de journalisation en Elixir (https://elixircasts.io/writing-and-reading-logs-in-elixir)
- Une discussion sur l'importance de la gestion des erreurs dans la programmation (https://www.justokay.co/elixir/error-handling-with-elixir)