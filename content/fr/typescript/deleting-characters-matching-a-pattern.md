---
title:    "TypeScript: Suppression de caractères correspondant à un motif"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Pourquoi

Souvent lors de la programmation, nous sommes confrontés à des chaînes de caractères qui peuvent contenir des caractères indésirables. Supprimer ces caractères peut être utile dans de nombreuses situations, comme nettoyer une entrée utilisateur ou formater des données pour une utilisation ultérieure. Dans cet article, nous allons apprendre comment supprimer des caractères correspondant à un modèle en utilisant TypeScript.

## Comment exécuter

Pour supprimer des caractères correspondant à un modèle, nous utiliserons la méthode `replace()` de TypeScript. Cette méthode prend deux paramètres: le premier est le modèle dont nous voulons supprimer les caractères et le deuxième est une chaîne de remplacement vide. Voici un exemple de code montrant comment utiliser la méthode `replace()`:

```
TypeScript
let str:string = "Bonjour Monde!"
let newStr:string = str.replace(/o/g, "")
console.log(newStr)
// Output: Bnjur Mnde!
```

Dans cet exemple, nous utilisons la méthode `replace()` pour supprimer tous les caractères "o" de la chaîne de caractères. Nous déclarons d'abord une variable `str` contenant la chaîne de caractères initiale et ensuite nous utilisons la méthode `replace()` pour retourner une nouvelle chaîne sans les caractères "o". Finalement, nous imprimons cette nouvelle chaîne avec `console.log()`.

## Plongée en profondeur

Il est important de noter que pour supprimer tous les caractères correspondant à un modèle, nous utilisons les expression régulières en plaçant un "g" à la fin du modèle dans la méthode `replace()`. Cela indique à TypeScript de rechercher et remplacer tous les caractères correspondant au modèle dans la chaîne de caractères. De plus, la méthode `replace()` ne modifie pas la chaîne initiale, mais retourne plutôt une nouvelle chaîne avec les modifications appliquées.

## Voir aussi

Vous pouvez consulter ces ressources pour en savoir plus sur les expressions régulières et la méthode `replace()` de TypeScript:

- [Documentation sur les expressions régulières pour TypeScript](https://www.typescriptlang.org/docs/handbook/regex.html)
- [Documentation sur la méthode `replace()` de TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#pattern-matching)