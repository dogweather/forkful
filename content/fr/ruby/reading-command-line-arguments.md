---
title:                "Lecture des arguments de ligne de commande"
html_title:           "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

Lire les arguments de la ligne de commande, c'est récupérer les informations que l'utilisateur envoie à votre programme lors de son exécution. C'est utile car cela permet à l'utilisateur de paramétrer l'exécution du programme selon ses besoins.

## Comment faire:

En Ruby, vous pouvez accéder aux arguments de la ligne de commande grâce à la variable globale ARGV. Par exemple:

```ruby
puts "Vous avez envoyé les arguments suivants : #{ARGV.join(', ')}"
```

Si vous exécutez votre programme avec `ruby votre_programme.rb arg1 arg2 arg3`, il affichera:

```ruby
Vous avez envoyé les arguments suivants: arg1, arg2, arg3
```

## Plongée en profondeur:

Historiquement, la lecture des arguments de la ligne de commande est une tradition des systèmes Unix, utilisée pour permettre la configuration des scripts et des programmes. En Ruby, `ARGV` est une constante qui est automatiquement alimentée par les arguments. Elle est en réalité une instance de la classe Array.

Il est également possible d'utiliser `ARGV` avec la bibliothèque OptionParser pour gérer des options de ligne de commande plus complexes, mais ce n'est pas nécessaire pour des cas simples.

## Voir aussi:

Vous trouverez davantage d'informations sur le sujet dans les sources suivantes :
- La documentation officielle de Ruby sur ARGV : https://docs.ruby-lang.org/en/3.0.0/ARGF.html
- La bibliothèque OptionParser : https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html
- Un guide sur la ligne de commande en Ruby : https://www.theodinproject.com/courses/ruby-programming/lessons/command-line-basics-web-development-101