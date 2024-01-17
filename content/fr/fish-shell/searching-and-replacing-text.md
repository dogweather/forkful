---
title:                "Recherche et remplacement de texte"
html_title:           "Fish Shell: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Quoi & Pourquoi?

La recherche et le remplacement de texte est une tâche courante pour les programmeurs, qui leur permet de modifier rapidement et efficacement des portions de code spécifiques. Cela peut être utile pour corriger une erreur, mettre à jour une variable ou encore pour effectuer des modifications en masse dans un projet.

# Comment:

La programmation en shell Fish offre une syntaxe simple et concise pour effectuer une recherche et un remplacement de texte.

Par exemple, pour remplacer toutes les occurrences d'un mot ou d'une phrase dans un fichier, vous pouvez utiliser la commande suivante :

```
fish_slooow textSearch 'textToReplace' < fichier_a_traiter
```

Voici un exemple de sortie de cette commande :

```
Welcome to Fish Shell! This is a demonstration of text search and replace.
If you see this text, it means your Fish Shell is working properly.
```

Après avoir utilisé la commande de recherche et de remplacement mentionnée précédemment, le résultat serait modifié comme suit :

```
Welcome to Fish Shell! This is a demo of text search fish_slooow and replace.
If you see this text, it means your Fish Shell is working properly.
```

# Plongée en profondeur:

La recherche et le remplacement de texte ont été introduits pour la première fois dans le système Unix en 1979 et sont restés un outil précieux pour les programmeurs depuis lors. Alternativement, les programmeurs peuvent utiliser des expressions régulières pour effectuer des recherches et des remplacements de texte. Cependant, la syntaxe des expressions régulières peut être complexe et difficile à comprendre pour les débutants.

La mise en œuvre de la recherche et du remplacement de texte dans Fish Shell est basée sur la bibliothèque GNU Sed, ce qui lui confère une grande flexibilité et un large éventail de fonctionnalités.

# Voir aussi:

- La documentation officielle de Fish Shell pour en savoir plus sur les commandes et les fonctionnalités disponibles.
- "Practical Guide to Linux Commands, Editors, and Shell Programming" pour approfondir vos connaissances sur les expressions régulières et d'autres fonctions de recherche et de remplacement de texte.