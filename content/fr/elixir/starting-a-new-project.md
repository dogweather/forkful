---
title:                "Elixir: Lancer un nouveau projet"
programming_language: "Elixir"
category:             "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a de nombreuses raisons pour lesquelles quelqu'un pourrait vouloir se lancer dans un nouveau projet de programmation en Elixir. Peut-être que vous cherchez à apprendre un nouveau langage de programmation ou à étendre vos compétences en programmation fonctionnelle. Ou peut-être que vous êtes simplement attiré par toutes les fonctionnalités intéressantes et dynamiques qu'Elixir a à offrir. Quelle que soit votre motivation, vous ne serez pas déçu d'explorer le monde de la programmation en Elixir.

## Comment faire

Pour commencer votre nouveau projet en Elixir, suivez ces étapes simples :

```Elixir
# Créer un nouveau fichier de projet
mix new mon_projet

# Entrer dans le dossier du projet
cd mon_projet

# Ouvrir le fichier d'application principal en utilisant Atom par exemple
atom lib/mon_projet.ex
```

Maintenant que vous avez créé votre projet, il est temps d'écrire votre premier code en Elixir. Voici un exemple de fonction qui prend en compte un nombre et retourne son double :

```Elixir
defmodule MonProjet do
  def double(n) do
    n * 2
  end
end

MonProjet.double(5)

# Output : 10
```

Vous pouvez également utiliser le mode interactif Elixir (en entrant la commande `iex` dans votre terminal) pour expérimenter avec du code en temps réel.

## Plongée en profondeur

Lorsque vous démarrez un nouveau projet en Elixir, il est important de comprendre quelques concepts clés tels que les modules, les fonctions, les types de données et les structures de contrôle. Vous devrez également apprendre à utiliser les nombreuses bibliothèques et frameworks disponibles pour Elixir, tels que Phoenix pour le développement d'applications web. Mais ne vous inquiétez pas, il y a une abondance de ressources en ligne pour vous aider à apprendre et à maîtriser Elixir.

## Voir aussi

- [Site officiel d'Elixir](https://elixir-lang.org/)
- [Documentation Elixir](https://hexdocs.pm/elixir/)
- [Codecademy : Apprenez Elixir](https://www.codecademy.com/learn/learn-elixir)
- [Elixir Forum (Forum de discussion sur Elixir)](https://elixirforum.com/)