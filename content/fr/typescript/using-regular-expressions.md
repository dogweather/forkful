---
title:                "Utilisation des expressions régulières."
html_title:           "TypeScript: Utilisation des expressions régulières."
simple_title:         "Utilisation des expressions régulières."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi

Si vous êtes développeur·se, vous avez probablement déjà entendu parler des expressions régulières, aussi appelées "regex". Elles sont très utiles pour rechercher, extraire et modifier des chaînes de caractères dans du texte. En bref, les regex peuvent vous faire économiser beaucoup de temps et d'efforts en automatisant des tâches répétitives. 

## Comment faire

Pour utiliser des expressions régulières en TypeScript, il suffit de les mettre entre deux barres obliques, comme ceci : `/regex/`. Vous pouvez également utiliser le constructeur `RegExp` pour créer des objets regex. Voici un exemple de code qui vérifie si une chaîne de caractères contient uniquement des lettres :

```TypeScript
const regex = /^[a-zA-Z]+$/;
const exampleString = "HelloWorld";
const result = regex.test(exampleString);
console.log(result); // Le résultat sera 'true'
```

Vous pouvez également utiliser des métacaractères pour créer des expressions plus complexes. Par exemple, le point `.` correspond à n'importe quel caractère et l'astérisque `*` signifie "0 ou plusieurs fois". Ci-dessous, nous vérifions si une chaîne de caractères est un numéro de téléphone au format américain :

```TypeScript
const regex = /^\d{3}-\d{3}-\d{4}$/;
const phoneNumber = "555-123-4567";
const result = regex.test(phoneNumber);
console.log(result); // Le résultat sera 'true'
```

## Plongée en profondeur

Les expressions régulières peuvent sembler ésotériques au premier abord, mais elles suivent en réalité des règles très précises. Voici quelques astuces pour bien les utiliser :

- Les crochets `[ ]` vous permettent de spécifier un ensemble de caractères. Par exemple, `[a-z]` correspond à n'importe quelle lettre minuscule.
- Les parenthèses `()` créent un groupe de caractères, utile pour des opérations de remplacement par exemple.
- Les métacaractères peuvent être combinés pour des résultats plus précis. Par exemple, `[^0-9]` correspondra à n'importe quel caractère sauf un chiffre.

Il existe de nombreux sites et outils en ligne pour tester vos regex et vous aider à les comprendre, n'hésitez pas à les utiliser !

## Voir aussi

- [Documentation officielle TypeScript sur les expressions régulières](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regex101 - un outil pour tester et créer des regex en ligne](https://regex101.com/)
- [The Coding Train - une chaîne YouTube avec de nombreuses vidéos sur les regex en JavaScript](https://www.youtube.com/user/shiffman)