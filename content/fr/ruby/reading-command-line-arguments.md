---
title:    "Ruby: Lecture des arguments de ligne de commande"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes un développeur Ruby, vous connaissez probablement l'importance d'interagir avec les utilisateurs à travers une interface en ligne de commande. La capacité de lire les arguments de ligne de commande peut vous aider à rendre vos programmes plus dynamiques et interactifs. Dans cet article, nous allons vous montrer comment lire des arguments de ligne de commande en utilisant Ruby.

## Comment faire

Pour lire des arguments de ligne de commande en Ruby, vous devez utiliser la variable spéciale `$ARGV` qui stocke les arguments fournis lors de l'exécution du programme. Voici un exemple de code qui montre comment lire et utiliser les arguments de ligne de commande :

```Ruby
# Exemple de code pour lire des arguments de ligne de commande
# ruby command_line_args.rb argument1 argument2

# Stockage des arguments dans un tableau grâce à la variable $ARGV
args = $ARGV

# Récupération des arguments
arg1 = args[0]
arg2 = args[1]

# Affichage des arguments
puts "Argument 1 : #{arg1}"
puts "Argument 2 : #{arg2}"
```

Si nous exécutons ce programme en lui fournissant les arguments `argument1` et `argument2`, voici le résultat que nous obtenons :

```
$ ruby command_line_args.rb argument1 argument2
Argument 1 : argument1
Argument 2 : argument2
```

Comme vous pouvez le constater, nous avons facilement pu accéder et utiliser les arguments fournis en utilisant la variable `$ARGV`. Vous pouvez également faire des opérations plus avancées avec ces arguments, comme les convertir en entiers ou effectuer des validations.

## Plongée en profondeur

En plongeant un peu plus en profondeur dans la lecture des arguments de ligne de commande en Ruby, vous remarquerez que la variable `$ARGV` n'est pas la seule façon de les récupérer. Vous pouvez également utiliser la méthode `ARGV.getopts` qui vous permettra de spécifier des options pour vos arguments. Par exemple, si vous voulez que votre programme accepte les arguments `-a` et `-b`, vous pouvez le faire comme suit :

```Ruby
# Exemple de code pour utiliser la méthode getopts
# ruby command_line_args.rb -a argument1 -b argument2

# Spécification des options à utiliser
opt = { 'a' => 'arg1', 'b' => 'arg2' }

# Utilisation de la méthode getopts pour récupérer les arguments
ARGV.getopts(opt) do |opt, val|
  if opt == 'a'
    arg1 = val
  elsif opt == 'b'
    arg2 = val
  end
end

# Affichage des arguments
puts "Argument 1 : #{arg1}"
puts "Argument 2 : #{arg2}"
```

En utilisant cette méthode, vous obtiendrez le même résultat que précédemment.

## Voir aussi

- Documentation officielle de Ruby sur la lecture des arguments de ligne de commande : https://ruby-doc.org/core-3.0.1/ARGF.html
- Tutoriel complet sur la manipulation des arguments de ligne de commande en Ruby : https://www.rubyguides.com/2018/11/ruby-command-line-arguments/
- Vidéo explicative sur les différentes façons de lire les arguments de ligne de commande en Ruby : https://www.youtube.com/watch?v=jVylMSdA6a0