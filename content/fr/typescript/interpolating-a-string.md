---
title:                "Interpoler une chaîne de caractères"
html_title:           "TypeScript: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi le faire ?
L'interpolation de chaîne (ou string interpolation en anglais) est une fonctionnalité courante en programmation qui permet d'insérer des valeurs de variables ou d'expressions dans une chaîne de caractères. Elle simplifie la création de chaînes de caractères dynamiques en évitant d'avoir à concaténer plusieurs éléments. Les programmeurs utilisent souvent cette technique pour formater et afficher des messages personnalisés.

## Comment faire :
Voici un exemple de code en TypeScript utilisant l'interpolation de chaîne :

```TypeScript
let nom = "John";
let age = 21;

console.log(`Bonjour ${nom}, vous avez ${age} ans.`);
```

Résultat :

```TypeScript
Bonjour John, vous avez 21 ans.
```

Dans cet exemple, nous avons utilisé des expressions entourées par des symboles « ${ } » pour insérer les valeurs de nos variables dans la chaîne.

## Plongée en profondeur :
L'interpolation de chaîne est une technique qui existe depuis de nombreuses années, mais elle est devenue plus populaire avec l'avènement de la programmation orientée objet. Auparavant, les programmeurs utilisaient souvent la concaténation de chaînes pour créer des messages dynamiques, mais cette méthode était fastidieuse et sujette aux erreurs.

Bien que l'interpolation de chaîne soit disponible dans de nombreux langages de programmation, chaque langage peut avoir sa propre syntaxe. Par exemple, en PHP, on utilise le signe « $ » devant le nom de la variable pour l'insérer dans une chaîne.

En termes d'alternatives, certains programmeurs préfèrent utiliser la méthode de formatage de chaîne, qui consiste à utiliser des placeholders et de fournir les valeurs séparément. Cependant, l'interpolation de chaîne est souvent considérée comme plus simple et intuitive.

## À voir aussi :
- [Documentation officielle de TypeScript sur l'interpolation de chaîne](https://www.typescriptlang.org/docs/handbook/basic-types.html#template-literals)
- [Autre article expliquant l'interpolation de chaîne en TypeScript](https://ourcodeworld.com/articles/read/480/how-to-use-string-interpolation-expresions-with-variables-inside-a-string-using-template-literals-in-typescript)