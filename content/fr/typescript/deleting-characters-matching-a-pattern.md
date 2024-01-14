---
title:                "TypeScript: Suppression de caractères correspondant à un motif"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

L'envie de supprimer des caractères correspondant à un motif peut survenir lors de la manipulation de données ou de chaînes de caractères. Cela peut également être utile dans le cadre de la validation ou du formatage de données saisies par l'utilisateur.

## Comment Faire

Pour supprimer des caractères correspondant à un motif en TypeScript, vous pouvez utiliser la méthode `replace()` avec une expression régulière comme motif. Voici un exemple de code qui supprime tous les caractères non alphabétiques d'une chaîne de caractères et renvoie le résultat :

```TypeScript
const string = "Hé% 1ll0!";
const pattern = /[^a-zA-Z]/g;
const result = string.replace(pattern, "");
console.log(result); // Résultat : "Héll"
```

Dans l'exemple ci-dessus, la méthode `replace()` a été utilisée avec deux arguments : le motif à rechercher (tous les caractères non alphabétiques) et la chaîne de remplacement vide (pour supprimer ces caractères).

Vous pouvez également accéder à la liste des caractères correspondant au motif en utilisant la méthode `match()` avec la même expression régulière :

```TypeScript
const string = "Hé% 1ll0!";
const pattern = /[^a-zA-Z]/g;
const result = string.match(pattern);
console.log(result); // Résultat : ["%", "1", "0"]
```

## Plongée Profonde

La méthode `replace()` en TypeScript utilise les expressions régulières pour rechercher et remplacer des motifs dans une chaîne de caractères. Les expressions régulières sont des séquences de caractères utilisées pour décrire un motif de recherche plutôt que des caractères littéraux. Elles offrent une grande flexibilité et puissance dans la manipulation de chaînes de caractères.

Pour en savoir plus sur les expressions régulières et comment les utiliser en TypeScript, vous pouvez consulter ces ressources :

- [Expressions régulières en TypeScript - Site officiel](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutoriel sur les expressions régulières en TypeScript - TutorialsTeacher](https://www.tutorialsteacher.com/typescript/regexp)
- [Introduction à JavaScript : expressions régulières - MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_r%C3%A9guli%C3%A8res)

## Voir aussi

- [Méthode `replace()` - Site officiel de TypeScript](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-1.html#string-methods)
- [Méthode `match()` - Site officiel de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#match)
- [Les expressions régulières en TypeScript pour le formatage de chaînes de caractères - Medium](https://medium.com/@sourabhkasudhan/typescript-regular-expressions-to-format-string-c28f3749d7a8)