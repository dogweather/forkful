---
title:    "Gleam: Supprimer les caractères correspondant à un motif"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

### Pourquoi

Supprimer des caractères correspondant à un motif peut être une tâche courante dans la programmation. Cela peut être utile lors de la suppression de données obscures ou de caractères inutiles dans une chaîne de texte. Dans cet article, nous allons expliquer comment le faire en utilisant le langage de programmation Gleam.

### Comment faire

Pour commencer, définissons un exemple de chaîne de texte avec laquelle nous allons travailler :

```
let texte = "Bonjour, je suis un texte avec plusieurs caractères inutiles!"
```

Nous pouvons alors utiliser la fonction `remove` de Gleam pour supprimer les caractères qui correspondent à un motif spécifique. Par exemple, si nous voulons supprimer tous les caractères non alphabétiques de notre chaîne de texte, nous pouvons utiliser le motif `alpha` :

```
let nouveau_texte = Texte.remove(texte, "alpha")
```

Et voici notre résultat :

```
"Bonjourjeuntexteavecplusieurscaractèresinutiles"
```

Nous pouvons également utiliser des expressions régulières pour définir un motif plus précis. Par exemple, si nous voulons supprimer tous les nombres de notre chaîne de texte, nous pouvons utiliser la regex `\\d` :

```
let nouveau_texte = Texte.remove(texte, "\\d")
```

Et voici notre résultat :

```
"Bonjour, je suis un texte avec plusieurs caractères inutiles!"
```

Il est important de noter que la fonction `remove` ne modifie pas la chaîne de texte d'origine, elle renvoie plutôt une nouvelle chaîne de texte sans les caractères correspondant au motif.

### Plongée profonde

En plongeant plus en profondeur, nous pouvons également utiliser la fonction `remove` pour supprimer des caractères spécifiques d'une chaîne de texte. Par exemple, si nous voulons supprimer tous les "x" de notre chaîne de texte, nous pouvons utiliser la syntaxe `[x -]` :

```
let nouveau_texte = Texte.remove(texte, "[x -]")
```

Et voici notre résultat :

```
"Bonjourjjeuiun teet ecut plusieurs caractères iutiles!"
```

### Voir aussi

- Documentation officielle de la fonction `remove` : https://gleam.run/modules/text#remove
- Tutoriel sur les expressions régulières en Gleam : https://gleam.run/articles/regular-expressions