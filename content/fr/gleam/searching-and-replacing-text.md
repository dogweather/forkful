---
title:    "Gleam: Recherche et remplacement de texte"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Pourquoi

Lorsque vous travaillez sur du code informatique, il est important de garder un œil attentif sur chaque ligne de texte. Mais que se passe-t-il si vous devez effectuer des modifications en masse sur un grand nombre de lignes ? Cela peut être fastidieux et prendre beaucoup de temps à faire manuellement. C'est là qu'entre en jeu la recherche et le remplacement de texte, une fonctionnalité utile pour automatiser ces tâches.

# Comment faire

La recherche et le remplacement de texte sont des fonctionnalités intégrées dans Gleam qui peuvent être utilisées facilement. Tout d'abord, vous devez définir le texte sur lequel vous souhaitez effectuer la recherche. Ensuite, vous pouvez utiliser la fonction `text.replace` pour remplacer ce texte par un autre. Par exemple :

```
Gleam
  |> text.replace("programmation", "codage")
```

Cela remplacera toutes les occurrences de "programmation" par "codage" dans le texte. Vous pouvez également utiliser cette fonction avec des expressions régulières pour une recherche et un remplacement plus précis.

# Plongée en profondeur

La fonction `text.replace` prend en entrée le texte sur lequel effectuer la recherche, le texte à rechercher, le texte de remplacement et un drapeau d'insensibilité à la casse, qui est facultatif. Vous pouvez également utiliser une autre fonction appelée `text.replace_slice` qui prend des indices de début et de fin pour spécifier une partie spécifique du texte à remplacer. Cela peut être utile si vous ne voulez pas remplacer toutes les occurrences du texte.

# Voir aussi

- [Documentation de la recherche et du remplacement de texte dans Gleam] (https://gleam.run/core_html_text-text.html#replace)
- [Guide de référence de Gleam] (https://gleam.run/about/docs.html)