---
title:    "Ruby: Lecture des arguments de ligne de commande"
keywords: ["Ruby"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Les arguments de ligne de commande sont un élément essentiel de la programmation Ruby. Ils permettent aux utilisateurs de passer des valeurs à un programme au moment de l'exécution, ce qui le rend plus flexible et personnalisable. Dans cet article, nous allons explorer comment lire ces arguments et les utiliser dans notre code Ruby.

## Comment faire

Pour lire les arguments de ligne de commande en Ruby, nous utilisons l'objet `ARGV`. Il s'agit d'un tableau contenant tous les arguments passés au programme. Voyons un exemple simple:

```Ruby
# ex1.rb
puts "Bonjour, #{ARGV[0]}!"
```

En exécutant ce programme avec `ruby ex1.rb Alice` depuis la ligne de commande, nous obtenons la sortie suivante:

```
Bonjour, Alice!
```

Comme vous pouvez le constater, nous avons utilisé la notation `#{}` pour inclure la valeur de `ARGV[0]` (qui correspond au premier argument passé) dans notre chaîne de caractères.

Nous pouvons également utiliser une boucle `each` pour parcourir tous les arguments et les utiliser dans notre code. Prenons par exemple ce programme qui affiche les arguments passés en majuscules:

```Ruby
# ex2.rb
ARGV.each do |arg|
  puts arg.upcase
end
```

En l'exécutant avec `ruby ex2.rb Hello world`, nous obtenons la sortie suivante:

```
HELLO
WORLD
```

Vous pouvez également combiner les arguments de ligne de commande avec d'autres concepts de Ruby, comme les conditions et les boucles. L'imagination est la seule limite.

## Plongée en profondeur

Maintenant que nous avons vu comment utiliser les arguments de ligne de commande, explorons quelques concepts plus avancés.

Tout d'abord, il est important de noter que tous les arguments de ligne de commande sont des chaînes de caractères. Si vous souhaitez utiliser des entiers ou des flottants, vous devrez les convertir en utilisant les méthodes `to_i` et `to_f` respectivement.

```Ruby
# ex3.rb
sum = 0
ARGV.each do |arg|
  sum += arg.to_i
end
puts "La somme est de #{sum}"
```

En l'exécutant avec `ruby ex3.rb 5 10`, nous obtenons la sortie suivante:

```
La somme est de 15
```

Un autre concept important à comprendre est la différence entre les arguments de ligne de commande et les options de ligne de commande. Les arguments sont des valeurs passées au programme, tandis que les options sont des indicateurs qui modifient le comportement du programme. Par exemple, l'option `-h` est couramment utilisée pour afficher un message d'aide et quitter le programme sans l'exécuter.

## Voir aussi

- [Documentation officielle sur les arguments de ligne de commande en Ruby](https://ruby-doc.org/core-2.7.3/ARGF.html)
- [Un tutoriel interactif sur les arguments de ligne de commande](https://www.codecademy.com/learn/learn-ruby/modules/learn-ruby-control-flow-u)
- [Un article sur les différentes façons d'utiliser les arguments de ligne de commande en Ruby](https://blog.appsignal.com/2020/02/19/arguments-and-flags-via-command-line-in-ruby.html)