---
aliases:
- /fr/ruby/starting-a-new-project/
date: 2024-01-20 18:04:17.199205-07:00
description: "D\xE9marrer un nouveau projet, c'est comme planter une graine num\xE9\
  rique : on l'arrose de code et elle pousse pour devenir une application. Les programmeurs\u2026"
lastmod: 2024-02-18 23:09:09.408087
model: gpt-4-1106-preview
summary: "D\xE9marrer un nouveau projet, c'est comme planter une graine num\xE9rique\
  \ : on l'arrose de code et elle pousse pour devenir une application. Les programmeurs\u2026"
title: Lancement d'un nouveau projet
---

{{< edit_this_page >}}

## What & Why? (Quoi & Pourquoi ?)
Démarrer un nouveau projet, c'est comme planter une graine numérique : on l'arrose de code et elle pousse pour devenir une application. Les programmeurs débutent des projets pour résoudre des problèmes, explorer des idées ou apprendre de nouvelles choses.

## How to: (Comment faire :)
Pour commencer, on va utiliser la gem 'bundler' qui plante cette graine pour nous.

```Ruby
# 1. Installez Bundler s'il n'est pas déjà installé
gem install bundler

# 2. Créez un nouveau dossier pour votre projet
mkdir mon_nouveau_projet
cd mon_nouveau_projet

# 3. Exécutez Bundler pour initialiser votre projet
bundle init

# À ce stade, un fichier Gemfile est créé dans votre dossier de projet.
```

Voici ce que vous verrez :

```
Writing new Gemfile to /chemin/vers/mon_nouveau_projet/Gemfile
```

Et voilà, la base est posée.

## Deep Dive (Plongée profonde)
Bundler existe depuis 2009. Avant cela, les Rubyistes géraient leurs gems à la main, parfois aboutissant à des cauchemars de dépendances. Alternatives ? RVM, rbenv peuvent aider mais ne sont pas équivalents à Bundler, qui gère spécifiquement les dépendances de votre projet.

L'implémentation clé de Bundler est le Gemfile. C'est là que vous déclarez les gems dont vous avez besoin, et Bundler s'occupe du reste. Ceci évite les conflits et assure que tous les développeurs sur le projet utilisent la même version des gems. 

## See Also (Voir Aussi)
Pour en savoir plus, consultez :

- [Bundler](https://bundler.io/)
- [RubyGems](https://rubygems.org/)
- Pour un guide complet Ruby, le [Well-Grounded Rubyist](https://www.manning.com/books/the-well-grounded-rubyist) est excellent.
