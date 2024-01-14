---
title:    "Elixir: Écriture vers l'erreur standard"
keywords: ["Elixir"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Pourquoi

L'écriture au flux d'erreur standard, également appelée stderr, est une tâche importante dans la programmation Elixir. Cela permet de recevoir des messages d'erreur ou de débogage lorsqu'un programme est en cours d'exécution. Sans cette pratique, il peut être plus difficile de comprendre et de résoudre les problèmes dans votre code.

## Comment faire

Voici un exemple simple de la syntaxe pour écrire au flux d'erreur standard en Elixir :

```Elixir
IO.puts("Ceci est un message de débogage") |> IO.puts(:stderr)
```

Cela affichera le message "Ceci est un message de débogage" dans le flux d'erreur standard. Vous pouvez également utiliser des variables dans la fonction `IO.puts` pour afficher des informations spécifiques, comme ceci :

```Elixir
age = 30
IO.puts("Votre âge est #{age}") |> IO.puts(:stderr)
```

Cela affichera "Votre âge est 30" dans le flux d'erreur standard. En utilisant cette méthode, vous pouvez facilement suivre le déroulement de votre code pour détecter et résoudre les erreurs.

## Plongée en profondeur

En écrivant au flux d'erreur standard, il faut également prendre en compte le niveau de gravité des messages. Par défaut, Elixir utilise trois niveaux : `:debug`, `:info`, et `:error`. Selon le niveau que vous choisissez, le message sera affiché avec une couleur et un format différents. Par exemple, si vous utilisez `IO.inspect` pour afficher un message de débogage, il sera affiché en vert avec le niveau `:debug`.

Vous pouvez également personnaliser le niveau de gravité en utilisant le module `Logger` :

```Elixir
Logger.debug("Message de débogage personnalisé") # Affiche en vert
Logger.error("Erreur personnalisée") # Affiche en rouge
```

Il est important de choisir le bon niveau de gravité pour vos messages afin de faciliter le processus de débogage.

## Voir aussi

- [Documentation Elixir : IO.puts](https://hexdocs.pm/elixir/IO.html#puts/2)
- [Documentation Elixir : Logger](https://hexdocs.pm/logger/Logger.html)
- [Utiliser Elixir pour le débogage](https://learnxinyminutes.com/docs/elixir-fr/)