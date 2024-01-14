---
title:    "PHP: Rechercher et remplacer du texte"
keywords: ["PHP"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/php/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de textes est une tâche très courante en programmation PHP. Cela peut être utile lorsque vous souhaitez modifier rapidement du contenu dans un grand nombre de fichiers, ou simplement pour mettre à jour des informations spécifiques dans votre code.

## Comment faire

La recherche et le remplacement de textes peuvent être facilement réalisés en utilisant les fonctions intégrées de PHP, telles que `str_replace()` ou `preg_replace()`. Voici un exemple de code pour remplacer tous les "hello" par "bonjour" dans une chaîne de caractères :

```PHP
<?php
$texte = "Hello, comment ça va?";
$texte_modifié = str_replace("hello", "bonjour", $texte);
echo $texte_modifié;
```

Ceci donnera en sortie "Bonjour, comment ça va?".

Vous pouvez également utiliser des expressions régulières avec la fonction `preg_replace()` pour effectuer une recherche et un remplacement plus avancés. Par exemple, pour remplacer tous les nombres dans une chaîne de caractères par des astérisques, vous pouvez utiliser le code suivant :

```PHP
<?php
$texte = "Il y a 25 chats dans la maison";
$texte_modifié = preg_replace("/\d+/", "***", $texte);
echo $texte_modifié;
```

Cela produira en sortie "Il y a *** chats dans la maison".

## Plongée en profondeur

Lors de l'utilisation de l'expression régulière avec `preg_replace()`, il est important de comprendre certains des symboles couramment utilisés. Par exemple, le symbole "/" à la fois avant et après le motif indique que c'est une expression régulière. Les symboles "+" et "*" représentent respectivement une ou plusieurs occurrences et zéro ou plusieurs occurrences du motif. Vous pouvez en apprendre davantage sur les expressions régulières en consultant la documentation officielle de PHP.

Il est également possible de combiner plusieurs fonctions, telles que `str_replace()` et `preg_replace()` pour effectuer des recherches et remplacements plus complexes et ciblés dans votre code.

## Voir aussi

- [Documentation officielle de PHP sur les expressions régulières](https://www.php.net/manual/fr/book.pcre.php)
- [Article sur la recherche et le remplacement de textes en PHP](https://blog.phpexperts.pro/text-search-and-replace-in-php/)