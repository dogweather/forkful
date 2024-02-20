---
date: 2024-01-26 04:17:07.400294-07:00
description: "Un shell interactif, ou REPL (Read-Eval-Print Loop, Boucle Lire-\xC9\
  valuer-Imprimer), vous permet de tester du code en temps r\xE9el. Les programmeurs\u2026"
lastmod: 2024-02-19 22:05:17.071378
model: gpt-4-0125-preview
summary: "Un shell interactif, ou REPL (Read-Eval-Print Loop, Boucle Lire-\xC9valuer-Imprimer),\
  \ vous permet de tester du code en temps r\xE9el. Les programmeurs\u2026"
title: Utilisation d'une console interactive (REPL)
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Un shell interactif, ou REPL (Read-Eval-Print Loop, Boucle Lire-Évaluer-Imprimer), vous permet de tester du code en temps réel. Les programmeurs l'utilisent pour expérimenter, déboguer et apprendre les subtilités de Ruby sans créer de scripts complets.

## Comment :
Le REPL de Ruby est appelé IRB (Interactive Ruby). Plongez dedans et essayez Ruby directement depuis votre terminal :

```Ruby
irb
2.7.0 :001 > puts "Bonjour, monde Ruby !"
Bonjour, monde Ruby !
 => nil
2.7.0 :002 > 5.times { print "Ruby ! " }
Ruby ! Ruby ! Ruby ! Ruby ! Ruby !  => 5
```

## Exploration approfondie
Introduit dans Ruby 1.8, IRB est un élément de base pour les Rubyistes. Il est inspiré par les shells interactifs de Lisp et Python, combinant l'expérimentation avec un retour immédiat. Des alternatives comme Pry offrent plus de fonctionnalités telles que la coloration syntaxique et un environnement de débogage plus robuste. IRB en lui-même est simple mais peut être augmenté avec des gems comme « irbtools » pour étendre sa fonctionnalité. La manière dont IRB gère la boucle lire-évaluer-imprimer est en lisant chaque ligne d'entrée, en l'évaluant comme du code Ruby, puis en imprimant le résultat, en répétant ce processus jusqu'à la sortie.

## Voir également
- [IRB de Ruby](https://ruby-doc.org/stdlib-2.7.0/libdoc/irb/rdoc/IRB.html)
- [Le gem irbtools](https://github.com/janlelis/irbtools)
