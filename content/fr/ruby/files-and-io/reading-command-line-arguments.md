---
date: 2024-01-20 17:56:47.320839-07:00
description: "Comment faire : Historiquement, les arguments de ligne de commande existent\
  \ depuis les premiers jours de l'informatique, car le terminal \xE9tait le moyen\u2026"
lastmod: '2024-04-05T22:51:12.291072-06:00'
model: gpt-4-1106-preview
summary: "Historiquement, les arguments de ligne de commande existent depuis les premiers\
  \ jours de l'informatique, car le terminal \xE9tait le moyen principal d\u2019interaction\
  \ avec les ordinateurs."
title: Lecture des arguments de ligne de commande
weight: 23
---

## Comment faire :
```Ruby
# script.rb
puts "Salut, #{ARGV[0]}!"
```
Exécuter en terminal:
```Bash
$ ruby script.rb Monde
```
Sortie:
```
Salut, Monde!
```

Vous pouvez aussi récupérer plusieurs arguments:
```Ruby
# multi_args.rb
puts "Arguments: #{ARGV.join(', ')}"
```
Exécuter:
```Bash
$ ruby multi_args.rb ces sont des arguments
```
Sortie:
```
Arguments: ces, sont, des, arguments
```

## Plongée en profondeur
Historiquement, les arguments de ligne de commande existent depuis les premiers jours de l'informatique, car le terminal était le moyen principal d’interaction avec les ordinateurs. En Ruby, `ARGV` est un tableau qui contient chaque argument passé au script, où ARGV[0] est le premier argument, ARGV[1] le second, et ainsi de suite. Alternativement, on peut utiliser la bibliothèque `OptionParser` pour une analyse plus complexe et des options typées. Il est important de valider et de nettoyer les entrées pour éviter les problèmes de sécurité comme les injections de commande.

## Voir Aussi
- Ruby Doc sur ARGV : [https://ruby-doc.org/core-2.7.0/ARGF.html](https://ruby-doc.org/core-2.7.0/ARGF.html)
- Un guide sur `OptionParser` : [https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html)
