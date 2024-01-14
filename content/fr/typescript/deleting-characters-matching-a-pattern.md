---
title:                "TypeScript: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi 

Il existe plusieurs raisons possibles pour lesquelles quelqu'un voudrait supprimer des caractères correspondant à un modèle dans leur code TypeScript. Peut-être qu'ils ont accidentellement entré des caractères non désirés, ou qu'ils mettent à jour un vieux code et doivent supprimer des parties inutiles.

## Comment faire

La suppression de caractères correspondant à un modèle peut être accomplie en utilisant des expressions régulières dans TypeScript. Voici un exemple de code pour supprimer tous les espaces dans une chaîne de caractères :

```TypeScript
let string = "Bonjour à   tous";
string = string.replace(/\s/g, '');
console.log(string); // Résultat: Bonjouràtous
```

Ce code utilise la méthode `replace()` pour remplacer tous les caractères correspondant à l'expression régulière `\s` (qui représente les espaces) par une chaîne de caractères vide. L'option `g` indique que la recherche doit être globale, c'est-à-dire qu'elle doit s'appliquer à toute la chaîne.

Un autre exemple serait de supprimer tous les chiffres d'une chaîne de caractères :

```TypeScript
let string = "ABC123DEF";
string = string.replace(/[0-9]/g, '');
console.log(string); // Résultat: ABCDEF
```

Dans cet exemple, l'expression régulière `[0-9]` représente tous les chiffres de 0 à 9, donc ils seront remplacés par une chaîne vide.

## Profondeur

Les expressions régulières peuvent sembler intimidantes au premier abord, mais une fois que l'on comprend leur syntaxe, elles peuvent être très puissantes pour la manipulation de chaînes de caractères. Voici quelques autres exemples d'expressions régulières couramment utilisées pour supprimer des caractères correspondant à un modèle :

- `.` représente n'importe quel caractère
- `+` représente un ou plusieurs occurrences du caractère précédent
- `*` représente zéro, un ou plusieurs occurrences du caractère précédent
- `^` représente le début d'une chaîne de caractères
- `$` représente la fin d'une chaîne de caractères

En utilisant ces symboles et d'autres combinaisons, vous pouvez créer des expressions régulières qui supprimeront des caractères spécifiques ou des modèles de caractères dans vos chaînes de caractères.

## Voir aussi

- [Documentation officielle de TypeScript sur les expressions régulières](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Editeur d'expressions régulières en ligne](https://regexr.com/)
- [Tutoriel sur les expressions régulières en anglais](https://www.regular-expressions.info/tutorial.html)