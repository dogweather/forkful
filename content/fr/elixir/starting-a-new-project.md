---
title:                "Démarrer un nouveau projet"
html_title:           "Elixir: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?
Lancer un nouveau projet en programmation, c'est simplement commencer un nouveau programme ou une nouvelle application. Les programmeurs font cela pour répondre à un besoin spécifique, pour améliorer une technologie existante ou simplement pour relever un défi personnel.

## Comment:
Pour démarrer un nouveau projet en Elixir, utilisez la commande ```mix new nom_du_projet```. Cela créera un nouveau dossier avec le nom du projet et une structure de base pour votre code. Ensuite, vous pouvez commencer à programmer en utilisant des modules, des fonctions et des structures de données propres à Elixir.

```Elixir
defmodule MonProjet do
  def greet(name) do
    "Bonjour #{name} !"
  end
end

IO.puts MonProjet.greet("Elixir")
```

Ce code créera un module "MonProjet" avec une fonction "greet" qui prend en paramètre un nom et retourne un message de salutation. Vous pouvez exécuter ce code en utilisant la commande ```elixir mon_projet.exs```dans le terminal.

## Plongée en profondeur:
Elixir a été créé en 2011 par José Valim avec pour objectif de fournir une alternative fiable et efficace à d'autres langages fonctionnels. Il est basé sur la machine virtuelle Erlang et peut facilement interagir avec le code écrit en Erlang. En plus d'être un langage fonctionnel, Elixir prend également en charge la programmation concurrente, ce qui en fait un excellent choix pour les applications en temps réel.

Pour démarrer un projet en Elixir, vous pouvez également utiliser d'autres outils tels que Mix (pour la gestion de projet), Hex (pour la gestion de paquets) et ExUnit (pour les tests unitaires). Ces outils font partie de l'écosystème Elixir et peuvent grandement faciliter le développement.

## Voir aussi:
- Site officiel d'Elixir: https://elixir-lang.org/
- Tutoriel sur Elixir: https://elixirschool.com/fr/
- Livre "Programming Elixir" de Dave Thomas: https://pragprog.com/book/elixir16/programming-elixir-1-6