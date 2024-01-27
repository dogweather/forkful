---
title:                "Modifier des fichiers avec des commandes CLI en une ligne"
date:                  2024-01-26T22:24:56.843812-07:00
model:                 gpt-4-0125-preview
simple_title:         "Modifier des fichiers avec des commandes CLI en une ligne"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data and Text Processing"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/ruby/modifying-files-with-cli-one-liners.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Modifier des fichiers avec des lignes de commande (CLI, Command Line Interface) en une seule ligne en Ruby consiste à effectuer rapidement et souvent simplement des manipulations de texte directement depuis le terminal en utilisant les options de ligne de commande de Ruby. Cette technique est inestimable lorsque vous avez besoin de faire des modifications en lot de fichiers, de filtrer du contenu ou d'automatiser des tâches d'édition sans ouvrir un éditeur. Il s'agit de tirer parti efficacement des capacités de traitement de texte de Ruby pour des modifications scriptables.

## Comment faire :
Imaginez que vous ayez un fichier nommé `example.txt` avec plusieurs lignes de texte et que vous souhaitiez inverser l'ordre des lignes. Avec Ruby, vous pouvez accomplir cela en une seule ligne :

```ruby
ruby -e 'puts File.readlines("example.txt").reverse' 
```

Ou, si vous souhaitez remplacer toutes les occurrences de "foo" par "bar" dans `data.txt`, vous pouvez faire :

```ruby
ruby -i.bak -pe 'gsub(/foo/, "bar")' data.txt
```

Cette commande crée également une sauvegarde (`data.txt.bak`) du fichier original, mettant en avant la considération de Ruby pour la sécurité des données. La sortie d'échantillon n'est pas directement visible puisque ces commandes changent le contenu du fichier, mais vous pouvez utiliser `cat data.txt` pour voir les changements.

## Plongée Profonde
Le drapeau `-e` indique à Ruby d'exécuter le script donné, tandis que `-i` active l'édition sur place avec une extension facultative pour créer un fichier de sauvegarde. Le drapeau `-p` parcourt l'entrée et imprime chaque ligne après que le script a été appliqué, semblable à sed dans Unix/Linux.

Historiquement, l'édition sur place et le traitement en ligne de commande étaient des territoires dominés par sed, awk et perl. Cependant, Ruby incorpore ces fonctionnalités de manière agréable, permettant des manipulations plus complexes en raison de sa syntaxe riche et de ses bibliothèques intégrées.

Les alternatives pour la modification de fichiers incluent sed et awk pour les tâches plus simples, ou l'utilisation de scripts Ruby complets pour un traitement plus complexe. L'inconvénient de l'utilisation de Ruby pour les lignes de commande peut être la performance pour les fichiers très volumineux ou les opérations complexes, où les outils conçus spécifiquement pour le traitement de texte pourraient être plus rapides.

En termes d'implémentation, lorsque Ruby traite les fichiers en ligne, il crée efficacement une sortie temporaire tout en lisant le fichier, puis remplace le fichier original par cette sortie. Ce détail souligne l'importance des options de sauvegarde ou des tests prudents avec l'utilisation du drapeau `-i` pour éviter la perte de données.

## Voir Aussi
- La documentation officielle de Ruby sur les options de ligne de commande : [https://www.ruby-lang.org/fr/documentation/quickstart/3/](https://www.ruby-lang.org/fr/documentation/quickstart/3/)
- Une comparaison approfondie du traitement de texte en Ruby par rapport à sed et awk : [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
- Pour une plongée plus profonde dans la gestion des fichiers et IO par Ruby : [https://ruby-doc.org/core-2.7.0/IO.html](https://ruby-doc.org/core-2.7.0/IO.html)
