---
title:                "Impression de sortie de débogage"
html_title:           "Elixir: Impression de sortie de débogage"
simple_title:         "Impression de sortie de débogage"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/elixir/printing-debug-output.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire?

Afficher des données de débogage est une pratique courante dans le développement de logiciels. Cela consiste à afficher des informations sur l'exécution du programme pour aider les programmeurs à comprendre comment leur code fonctionne et à résoudre les bugs. Les programmeurs le font pour trouver rapidement des erreurs et améliorer leurs programmes.

## Comment faire:

Voici un exemple simple en Elixir pour afficher une chaîne de caractères de débogage dans la console:

```Elixir
IO.inspect("Débogage en cours...")
```

La sortie sera: "Débogage en cours..."

Pour afficher plus d'informations, vous pouvez utiliser la directive de débogage ```inspect/2``` et lui passer une expression et des options:

```Elixir
IO.inspect(my_variable, label: "Valeur de la variable")
```

La sortie ressemblera à ceci: "Valeur de la variable: my_variable"

## Plongée en profondeur:

Afficher des données de débogage a été une pratique courante depuis le développement de langages de programmation et d'outils de débogage. De nos jours, il existe des alternatives plus avancées telles que les systèmes de débogage en temps réel et les systèmes de journalisation. Cependant, l'affichage de données de débogage reste une méthode simple et efficace pour comprendre le fonctionnement des programmes.

Spotlight est un outil Elixir avancé conçu pour faciliter le débogage en temps réel. Il permet aux développeurs de suivre l'exécution de leur code en temps réel sans avoir besoin d'ajouter des directives de débogage.

## Voir aussi:

Pour en savoir plus sur l'utilisation des directives de débogage en Elixir, consultez la documentation officielle d'Elixir: [IO.inspect](https://hexdocs.pm/elixir/IO.html#inspect/2)

Pour en savoir plus sur Spotlight, consultez leur site officiel: [Spotlight](https://hexdocs.pm/spotlight/overview.html)