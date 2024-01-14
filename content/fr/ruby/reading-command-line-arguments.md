---
title:                "Ruby: Lecture des arguments de ligne de commande"
simple_title:         "Lecture des arguments de ligne de commande"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Pourquoi lire les arguments de la ligne de commande en Ruby
La lecture des arguments de la ligne de commande est une compétence essentielle pour tout programmeur Ruby. Cela vous permet de passer des paramètres à votre programme lors de son exécution, ou de fournir des informations supplémentaires au programme pendant son exécution. Cela peut également vous aider à automatiser certaines tâches et à rendre votre code plus flexible. Dans cet article, nous allons couvrir les bases de la lecture des arguments de la ligne de commande en Ruby.

## Comment faire
Pour lire les arguments de la ligne de commande en Ruby, vous pouvez utiliser la variable globale `ARGV`. Cette variable contient un tableau avec tous les arguments passés à votre programme lors de son exécution. Voici un exemple :

```Ruby
# code exemple
ARGV.each do |arg|
  puts "Argument passé : #{arg}"
end
```

Si vous exécutez le programme ci-dessus en tapant `ruby mon_programme.rb un_argument deux_arguments`, vous obtiendrez la sortie suivante :

```
Argument passé : un_argument
Argument passé : deux_arguments
```

Comme vous pouvez le voir, chaque argument est stocké comme une chaîne de caractères dans le tableau `ARGV`.

## Plongée en profondeur
Il y a quelques points importants à garder à l'esprit lors de la lecture des arguments de la ligne de commande en Ruby. Tout d'abord, la première valeur du tableau `ARGV` sera toujours le nom du fichier Ruby que vous exécutez. Cela peut être utile si vous devez spécifier des fichiers d'entrée ou de sortie dans votre programme.

Deuxièmement, les arguments passés à votre programme seront séparés par des espaces. Si vous avez besoin de passer des arguments contenant des espaces, vous devrez les entourer de guillemets doubles (") ou simples ('). Sinon, les espaces seront considérés comme des séparateurs et vos arguments seront incorrects.

Enfin, vous pouvez également spécifier des options courtes et longues en utilisant la gem `optparse`. Cela vous permettra de définir des options avec des noms explicites et de les utiliser dans votre programme. Vous pouvez en savoir plus sur la gem `optparse` en consultant la [documentation officielle](https://ruby-doc.org/stdlib-2.7.0/libdoc/optparse/rdoc/OptionParser.html).

# Voir aussi
- [Documentation sur la variable globale ARGV](https://ruby-doc.org/core/ARGF.html)
- [Tutoriel sur la gem optparse](https://www.rubyguides.com/2018/10/ruby-optionsparser/)
- [Autre exemple de lecture des arguments de la ligne de commande en Ruby](https://www.devdungeon.com/content/ruby-getting-command-line-arguments)