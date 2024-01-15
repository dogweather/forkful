---
title:                "La lecture des arguments de ligne de commande"
html_title:           "Ruby: La lecture des arguments de ligne de commande"
simple_title:         "La lecture des arguments de ligne de commande"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Pourquoi

Pourquoi devriez-vous vous intéresser à la lecture des arguments de la ligne de commande ? C'est simple : c'est un moyen efficace d'interagir avec votre programme. En utilisant des arguments en ligne de commande, vous pouvez personnaliser l'exécution de votre programme et le rendre plus polyvalent.

## Comment faire

La lecture des arguments de la ligne de commande en Ruby est simple et peut être réalisée en quelques étapes faciles.

Tout d'abord, vous devez accéder aux arguments de la ligne de commande en utilisant la variable spéciale `$ARGUMENTS` (ou `$ARGV`). Cette variable contient un tableau de toutes les valeurs passées en tant qu'arguments lors de l'exécution du programme.

Ensuite, vous pouvez parcourir le tableau pour accéder à chaque argument individuellement. Par exemple :

```ruby
# Exemple d'utilisation des arguments en ligne de commande
# ruby mon_programme.rb argument1 argument2 argument3

# Accéder aux arguments en ligne de commande
arguments = ARGV

# Utiliser le premier argument
puts "Le premier argument est : #{arguments[0]}"
```

Output : `Le premier argument est : argument1`

Vous pouvez également utiliser la méthode `join` pour rassembler tous les arguments en une seule chaîne de caractères :

```ruby
# Exemple d'utilisation de la méthode 'join'
# ruby mon_programme.rb argument1 argument2 argument3

# Accéder aux arguments en ligne de commande
arguments = ARGV

# Rassembler tous les arguments en une seule chaîne de caractères
arguments_string = arguments.join(" ")

# Afficher la chaîne de caractères formée par les arguments
puts "Les arguments passés sont : #{arguments_string}"
```

Output : `Les arguments passés sont : argument1 argument2 argument3`

Et voilà ! Vous savez maintenant comment lire et utiliser les arguments de la ligne de commande en Ruby.

## Approfondissement

Maintenant que vous savez comment utiliser les arguments en ligne de commande, vous pouvez également explorer des options plus avancées telles que la validation des arguments, l'utilisation de librairies spécifiques pour gérer les arguments, ou encore créer des options d'aide pour votre programme.

Pour en savoir plus, vous pouvez consulter ces ressources :

- [Documentation officielle de Ruby sur les arguments de la ligne de commande](https://ruby-doc.org/core-2.7.2/ARGF.html)
- [Un tutoriel sur l'utilisation des arguments en ligne de commande en Ruby](https://www.rubyguides.com/2018/10/ruby-command-line-arguments/)
- [Une librairie Ruby pour gérer facilement les arguments en ligne de commande](https://github.com/docopt/docopt.rb)

## A voir aussi

Découvrez également ces autres articles sur Ruby :

- [Comment créer une classe en Ruby](https://www.ssbloggingtips.com/create-class-ruby/)
- [Guide pratique pour la manipulation de chaînes de caractères en Ruby](https://www.aaronlasseigne.com/2016/10/25/strscan-ruby-secret-weapon/)
- [Une introduction à la programmation orientée objet en Ruby](https://www.tutorialspoint.com/ruby/ruby_object_oriented.htm)