---
title:    "Fish Shell: Suppression de caractères correspondant à un motif."
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

### Pourquoi

Vous vous demandez peut-être pourquoi il serait utile de supprimer des caractères correspondant à un modèle dans le Shell Fish ? La réponse est simple : cela peut grandement faciliter et accélérer votre travail lorsque vous manipulez des fichiers et des données.

### Comment faire

Pour supprimer des caractères correspondant à un modèle dans le Shell Fish, vous pouvez utiliser la fonction intégrée `string replace`. Voici un exemple de code utilisant cette fonction :

```Fish Shell
set my_string "Bonjour, je m'appelle Marie"
string replace 'e' '' $my_string
```

Ce code va supprimer toutes les occurrences de la lettre "e" dans la variable `my_string` et afficher le résultat: "Bonjour, j'appelll Mar1. 

Vous pouvez également utiliser des modèles plus complexes en utilisant des expressions régulières. Voici un exemple de code supprimant tous les chiffres dans une chaîne de caractères :

```Fish Shell
set my_string "J'ai 35 ans"
string replace -r '[0-9]' '' $my_string
```

Le résultat sera: "J'ai ans". Comme vous pouvez le constater, cette fonction peut être très pratique pour nettoyer et manipuler des chaînes de caractères.

### Plongée en profondeur

La fonction `string replace` possède plusieurs options pour vous permettre de personnaliser votre remplacement de caractères. Vous pouvez par exemple modifier la casse en utilisant l'option `-l` pour passer en minuscules ou `-u` pour passer en majuscules. Vous pouvez également utiliser l'option `-c` pour supprimer des caractères spécifiques plutôt que de les remplacer.

De plus, vous pouvez combiner plusieurs modèles en utilisant des expressions régulières dans la fonction `string replace`. Par exemple, vous pouvez supprimer à la fois les lettres et les chiffres dans une chaîne de caractères en utilisant le modèle `'[a-z]|[0-9]'`.

### Voir aussi

- Documentation officielle de la fonction `string replace` dans le Shell Fish : https://fishshell.com/docs/current/cmds/string.html#string-replace
- Tutoriel sur l'utilisation des expressions régulières dans le Shell Fish : https://fishshell.com/docs/current/tutorial.html#regular-expressions
- Exemples d'utilisation de la fonction `string replace` : https://everythinglinux.org/regular_expressions_1.html#section_1

Profitez de cette fonctionnalité du Shell Fish pour vous simplifier la vie lors de vos manipulations de données et de fichiers !