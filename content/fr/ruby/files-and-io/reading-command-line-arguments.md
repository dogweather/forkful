---
date: 2024-01-20 17:56:47.320839-07:00
description: "Lire les arguments de la ligne de commande, c'est r\xE9cup\xE9rer des\
  \ donn\xE9es directement du terminal lors de l'ex\xE9cution d'un script. Les programmeurs\
  \ le font\u2026"
lastmod: '2024-03-13T22:44:58.437205-06:00'
model: gpt-4-1106-preview
summary: "Lire les arguments de la ligne de commande, c'est r\xE9cup\xE9rer des donn\xE9\
  es directement du terminal lors de l'ex\xE9cution d'un script. Les programmeurs\
  \ le font\u2026"
title: Lecture des arguments de ligne de commande
---

{{< edit_this_page >}}

## Quoi et Pourquoi ?
Lire les arguments de la ligne de commande, c'est récupérer des données directement du terminal lors de l'exécution d'un script. Les programmeurs le font pour personnaliser l'exécution sans modifier le code.

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
