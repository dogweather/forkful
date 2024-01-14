---
title:    "Gleam: Recherche et remplacement de texte"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

# Pourquoi

Vous avez peut-être déjà été confronté à la tâche fastidieuse de chercher et remplacer du texte dans votre code. Peut-être que vous avez accidentellement utilisé la mauvaise variable partout dans votre projet et vous devez maintenant corriger toutes les occurrences. Ou peut-être que vous changez de nom de variable pour plus de cohérence. Quelle que soit la raison, cela peut être un travail fastidieux et fastidieux. Mais heureusement, Gleam a une solution simple pour cela.

# Comment faire

Voici un exemple de code Gleam pour effectuer une recherche et un remplacement dans une chaîne de caractères :

```Gleam
fn remplacer_string(s: String) -> String {
  let nouveau_s = replace(s, "vieux", "nouveau")
  nouveau_s
}
```

Lorsque nous appelons la fonction avec une chaîne de caractères contenant le mot "vieux", il sera remplacé par "nouveau" dans le résultat. Voici un exemple du résultat :

```
remplacer_string("C'est une vieille méthode.") // "C'est une nouvelle méthode."
```

De même, vous pouvez remplacer des caractères spécifiques ou même des expressions régulières en utilisant la fonction `replace` dans Gleam.

# Plongée en profondeur

La fonction `replace` prend trois arguments : la chaîne de caractères d'origine, la chaîne de caractères à remplacer et la chaîne de caractères de remplacement. Elle renvoie la chaîne de caractères modifiée avec toutes les occurrences de la chaîne à remplacer remplacées par la chaîne de remplacement. La fonction `replace` fait partie de la bibliothèque standard de Gleam, elle est donc disponible pour une utilisation immédiate dans vos projets.

Vous pouvez également utiliser d'autres fonctions de remplacement de chaînes, telles que `replace_first` et `replace_last` qui permettent de remplacer respectivement la première et la dernière occurrence de la chaîne à remplacer dans la chaîne d'origine.

# Voir aussi

- La documentation de la fonction replace dans la bibliothèque standard de Gleam - https://gleam.run/documentation/stdlib/replace
- Un tutoriel étape par étape sur la recherche et le remplacement dans Gleam - https://gleam.run/tutorials/finding-and-replacing-text
- Une vidéo sur l'utilisation de la fonction replace dans un projet Gleam - https://www.youtube.com/watch?v=2jFDEA4NPjA