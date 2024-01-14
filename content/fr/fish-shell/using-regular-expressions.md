---
title:                "Fish Shell: Utiliser des expressions régulières"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous travaillez avec des chaînes de caractères dans votre code, il est probable que vous ayez rencontré des situations où vous deviez trouver et manipuler des motifs spécifiques dans ces chaînes. Les expressions régulières peuvent être un outil puissant pour accomplir cette tâche.

## Comment faire

Les expressions régulières sont des séquences spéciales de caractères qui représentent des modèles que l'on veut trouver dans une chaîne de caractères. Dans Fish Shell, vous pouvez utiliser les expressions régulières en utilisant la commande "grep".

Par exemple, si vous voulez trouver toutes les occurrences de "fish" dans une chaîne, vous pouvez utiliser la commande suivante :

```
Fish Shell grep "fish" texte.txt
```

Cela recherchera et affichera toutes les lignes de "texte.txt" qui contiennent le mot "fish". Vous pouvez également utiliser des symboles spéciaux comme "*" pour représenter plusieurs caractères ou "?" pour représenter un seul caractère.

## Plongée profonde

En plus d'utiliser les expressions régulières pour trouver des motifs spécifiques, vous pouvez également les utiliser pour remplacer ou modifier des parties de chaînes de caractères. Par exemple, si vous voulez remplacer toutes les occurrences de "fish" par "shell" dans une chaîne, vous pouvez utiliser la commande "sed" avec une expression régulière :

```
Fish Shell sed -i "s/fish/shell/g" texte.txt
```

Cela remplacera toutes les occurrences de "fish" par "shell" dans "texte.txt". Vous pouvez également combiner plusieurs expressions régulières pour créer des motifs plus complexes pour vos recherches et remplacements.

## Voir aussi

- [Documentation officielle de Fish Shell](https://fishshell.com/docs/current/index.html)
- [Tutoriel d'expressions régulières en français](https://openclassrooms.com/fr/courses/1567291-programmez-en-oriente-objet-en-php/1567593-les-expressions-regulieres)
- [Documentation officielle de grep](https://www.gnu.org/software/grep/manual/grep.html)