---
title:                "Elixir: Écrire sur la sortie d'erreur standard"
simple_title:         "Écrire sur la sortie d'erreur standard"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

# Pourquoi écrire vers l'erreur standard en Elixir

L'écriture vers l'erreur standard, aussi appelée stderr, est une étape importante lorsque l'on écrit du code en Elixir. Cela permet de mieux comprendre et de déboguer les erreurs qui se produisent lors de l'exécution d'un programme. C'est un outil précieux pour tout programmeur en Elixir.

## Comment faire

Pour écrire vers l'erreur standard en Elixir, il suffit d'utiliser la fonction `IO.puts/2` en lui passant le message d'erreur en premier argument et le flux d'erreur standard en deuxième argument.

```
Elixir IO.puts("Une erreur est survenue.", :stderr)
``` 

Cela affichera le message d'erreur dans le terminal, ce qui peut être très utile lors de la phase de débogage. Voici un exemple de sortie :

```
Une erreur est survenue.
```

## Plongée en profondeur

En Elixir, il est possible d'utiliser un logger pour gérer les erreurs et les messages de façon plus détaillée. Pour écrire vers l'erreur standard en utilisant un logger, il faut utiliser la fonction `Logger.error/1` en lui passant le message d'erreur en argument.

```
Elixir Logger.error("Une erreur est survenue.")
```

Cela affichera le message d'erreur dans le terminal ainsi que dans le fichier de log défini pour le logger en question. Il est également possible de personnaliser les informations affichées en utilisant différentes macros fournies par le module `Logger`.

## Voir aussi

- [Documentation Elixir sur l'écriture vers l'erreur standard](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Documentation Elixir sur les loggers](https://hexdocs.pm/logger/)

Merci d'avoir lu cet article et j'espère que cela vous a aidé à mieux comprendre l'utilité de l'écriture vers l'erreur standard en Elixir. N'hésitez pas à explorer davantage les différentes fonctionnalités et options disponibles pour gérer les erreurs dans vos programmes en Elixir.