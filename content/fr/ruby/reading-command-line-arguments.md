---
title:                "Ruby: Lecture des arguments de la ligne de commande"
programming_language: "Ruby"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

L'utilisation d'arguments en ligne de commande est un aspect important de la programmation en Ruby, car cela permet aux utilisateurs de passer des paramètres à un programme lorsqu'il est exécuté. Ce sont ces arguments qui peuvent rendre un programme plus flexible et permettre l'automatisation des processus.

## Comment faire

La lecture des arguments en ligne de commande dans Ruby est assez simple et peut être réalisée en quelques étapes faciles. Tout d'abord, nous devons utiliser la variable globale `ARGV` pour stocker tous les arguments entrés par l'utilisateur. Ensuite, nous pouvons utiliser des méthodes de manipulation de tableaux, telles que `shift` ou `slice`, pour extraire les arguments individuels du tableau.

```Ruby
# Exemple de code pour lire les arguments en ligne de commande
ARGV.each do |arg|
  puts "Argument : #{arg}"
end

# Output de la commande : ruby arguments.rb arg1 arg2 arg3
# Output du code ci-dessus :
# Argument : arg1
# Argument : arg2
# Argument : arg3
```

Vous pouvez également utiliser des options telles que `OptionParser`, qui vous permettent de définir des options avec des valeurs spécifiques pour vos arguments en ligne de commande. Cela peut être utile pour spécifier des paramètres obligatoires ou facultatifs pour votre programme.

```Ruby
# Exemple de code avec OptionParser
require 'optparse'

options = {}
OptionParser.new do |opts|
  opts.on("-f", "--format FORMAT", "Specify output format") do |format|
    options[:format] = format
  end
end.parse!

puts "Output format : #{options[:format]}"

# Output de la commande : ruby arguments.rb -f csv
# Output du code ci-dessus :
# Output format : csv
```

## Plongée en profondeur

En plus de ces méthodes de base, il existe également des bibliothèques tierces telles que `ARGV Wizard` qui offrent des fonctionnalités avancées pour la lecture des arguments en ligne de commande. Elles peuvent inclure des validations et des options de configuration supplémentaires pour améliorer encore plus l'expérience de l'utilisateur.

Il est également important de noter que les arguments en ligne de commande peuvent être utilisés avec d'autres aspects du langage Ruby, tels que les scripts ou les programmes en ligne de commande. Cela les rend extrêmement polyvalents et utiles pour une variété de projets.

## Voir aussi

- [Guide Ruby sur les arguments en ligne de commande](https://www.rubyguides.com/2018/06/ruby-argv/)
- [Documentation officielle de Ruby sur les options de ligne de commande](https://ruby-doc.org/core-2.6/OptionParser.html)
- [RubyGems : ARGV Wizard](https://rubygems.org/gems/argv_wizard)