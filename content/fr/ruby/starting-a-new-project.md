---
title:                "Démarrer un nouveau projet"
html_title:           "Elm: Démarrer un nouveau projet"
simple_title:         "Démarrer un nouveau projet"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/starting-a-new-project.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi?

Démarrer un nouveau projet en programmation consiste à établir l'ossature et l'orientation de votre futur logiciel. Les développeurs le font pour construire une base solide sur laquelle développer leur idée.

## Comment faire:

Pour créer un nouveau projet Ruby, utiliser l'outil `bundler` est souvent une bonne idée. Voyez comment:

```Ruby
# Installez bundler si vous ne l'avez pas encore
gem install bundler

# Créez un nouveau répertoire pour votre projet
mkdir mon_projet
cd mon_projet

# Créez un nouveau projet gem avec bundler
bundle gem mon_projet
```

Cela va générer l'arborescence de base pour votre projet, y compris le fichier `.gemspec` pour la configuration de la gem.

## Plongeon profond

Historiquement, les développeurs construisaient chaque projet à partir de zéro. Cependant, cela s'avérait inefficace et prédisposait à des erreurs. Ainsi, l'usage des "squelettes" de projet est devenu courant.

Des alternatives à `bundler` existent, comme `hoe` ou `ore`, mais Bundler reste l'outil privilégié de la majorité des développeurs Ruby grâce à sa simplicité et sa flexibilité.

L'implémentation détaillée d'un nouveau projet dépend de divers facteurs, comme la complexité du projet, les exigences spécifiques et l'équipe de développement. 

## Voir aussi

- La documentation officielle de Bundler: [Bundler.io](https://bundler.io/)
- Guide de démarrage rapide Ruby: [RubyQuickstart](https://www.ruby-lang.org/fr/documentation/quickstart/)
- Alternatives à Bundler : [Hoe](http://www.zenspider.com/projects/hoe.html), [Ore](https://github.com/ruby-ore/ore)