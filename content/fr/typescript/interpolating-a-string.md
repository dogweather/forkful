---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est & Pourquoi ?

L'interpolation de chaînes est le processus d'insertion de variables ou d'expressions dans une chaîne de caractères. C'est une pratique courante pour rendre le code plus propre, plus lisible et plus réutilisable.

## Comment faire :

En TypeScript, nous utilisons les backticks (`) pour créer une chaîne de caractères, et nous insérons les variables avec ${}. Regardez cet exemple :
```TypeScript
let prenom = "Marie";
let salutation = `Bonjour, ${prenom}!`;
console.log(salutation);  // "Bonjour, Marie!"
```
Dans cet exemple, la valeur de la variable `prenom` est insérée dans la chaîne `salutation`.

## Analyse en profondeur

Historiquement, en JavaScript, vous deviez construire ou concaténer des chaînes avec l'opérateur +, ce qui rendait le code encombrant et difficile à lire. L'interopération de chaîne, introduit avec ES6, a résolu ce problème.

Comme alternative à l'interpolation de chaînes, vous pouvez toujours utiliser la concaténation, mais cela peut conduire à un code plus verbosité et moins lisible.

Au niveau de l'implémentation, l'interpolation de chaîne est mise en œuvre comme une façon de formater les chaînes à l'aide des modèles de littéraux de chaîne. La notation de la variable `${nom_variable}` est placée à l'endroit où la valeur de la variable doit apparaître dans la chaîne.

## Voir aussi

Pour plus d'informations sur l'interpolation de chaînes en TypeScript, référez-vous à ces sources supplémentaires :


- Un tutoriel complet sur l'interpolation de chaînes avec des exemples : [https://www.tutorialsteacher.com/typescript/typescript-string](https://www.tutorialsteacher.com/typescript/typescript-string)