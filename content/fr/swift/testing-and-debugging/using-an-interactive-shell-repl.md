---
date: 2024-01-26 04:17:57.607268-07:00
description: "L'utilisation d'une coquille interactive, ou d'une boucle de lecture-\xE9\
  valuation-impression (REPL), vous permet de coder de mani\xE8re interactive. Les\u2026"
lastmod: '2024-03-11T00:14:32.106174-06:00'
model: gpt-4-0125-preview
summary: "L'utilisation d'une coquille interactive, ou d'une boucle de lecture-\xE9\
  valuation-impression (REPL), vous permet de coder de mani\xE8re interactive. Les\u2026"
title: Utilisation d'une console interactive (REPL)
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
L'utilisation d'une coquille interactive, ou d'une boucle de lecture-évaluation-impression (REPL), vous permet de coder de manière interactive. Les programmeurs l'utilisent pour tester rapidement des extraits de Swift, déboguer ou apprendre le langage.

## Comment faire :
Invoquez REPL en ouvrant un terminal et en exécutant `swift`. Tapez directement le code et appuyez sur Entrée pour l'exécuter. Voici un avant-goût :

```Swift
1> let greeting = "Bonjour, REPL !"
greeting: String = "Bonjour, REPL !"
2> print(greeting)
Bonjour, REPL !
```

Sortez avec `:quit` ou `Control-D`.

## Plongée Profonde
Les racines du REPL remontent aux interpréteurs Lisp dans les années 60. Le REPL de Swift repose sur LLVM, un cadre de compilation puissant, offrant plus qu'une simple interprétation - c'est un outil complet avec autocomplétion, débogage, et plus encore. REPL est excellent pour l'apprentissage ou le prototypage, mais ce n'est pas un environnement de développement autonome. Certaines personnes préfèrent utiliser les Playgrounds dans Xcode pour une approche plus graphique et basée sur des fichiers, tandis que d'autres s'en tiennent à l'édition de script traditionnelle et à son exécution.

Sous le capot, le REPL de Swift compile dynamiquement le code en langage machine et l'exécute, c'est pourquoi il est relativement rapide. Il peut également accéder à tout module Swift compilé, ou même à des bibliothèques C, le rendant assez puissant. Notez, cependant, que tout ne fonctionne pas parfaitement dans REPL ; certaines fonctionnalités de Swift, en particulier celles nécessitant des configurations de projet complexes ou des fichiers de storyboard, ne fonctionneront pas ici.

## Voir Aussi
- [Swift.org - Commencer](https://www.swift.org/getting-started/#using-the-repl)
- [Introduction aux Playgrounds Xcode par Apple](https://developer.apple.com/videos/play/wwdc2014/408/)
- [Projet LLVM](https://llvm.org/)
