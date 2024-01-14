---
title:    "TypeScript: Extraction de sous-chaînes"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Pourquoi

L'extraction de sous-chaînes est une tâche courante dans la programmation. Elle consiste à extraire une partie d'une chaîne de caractères en fonction de certains critères, tels qu'un index de début et de fin ou un motif spécifique. Cette opération peut être utile dans de nombreuses situations, telles que la validation des entrées utilisateur ou la manipulation de données.

## Comment faire

Pour extraire des sous-chaînes en TypeScript, utilisez la méthode `.slice()` ou la syntaxe d'index de tableau `[indexDépart:indexFin]`. Voici un exemple de code qui extrait une sous-chaîne d'une chaîne de caractères:

```TypeScript
const maChaine = "Voici une chaîne de caractères";
const sousChaine = maChaine.slice(6, 12); // "une ch"
```

ou

```TypeScript
const maChaine = "Voici une autre chaîne de caractères";
const sousChaine = maChaine[5:11]; // "une au"
```

Si vous souhaitez extraire une sous-chaîne en fonction d'un motif spécifique, utilisez la méthode `.substring()` en spécifiant le début et la longueur de la sous-chaîne souhaitée. Par exemple:

```TypeScript
const maChaine = "Motifs: cercle, carré, triangle";
const sousChaine = maChaine.substring(8, 6); // "carré"
```

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, vous pouvez également utiliser la méthode `.substr()` pour extraire une sous-chaîne en spécifiant le début et le nombre de caractères. Il est également possible d'extraire une sous-chaîne à partir de la fin d'une chaîne en utilisant des valeurs de début et de fin négatives.

De plus, TypeScript offre des fonctions de chaîne de caractères telles que `.trim()` pour supprimer les espaces blancs avant et après la chaîne, et `.toLowerCase()` et `.toUpperCase()` pour changer la casse des caractères.

Il est important de noter que ces méthodes et fonctions sont sensibles à la casse. Par conséquent, faites attention à la manière dont vous utilisez ces outils dans votre code.

## Voir aussi

- [Documentation TypeScript sur les opérations de chaîne de caractères](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Guide sur les sous-chaînes en JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Text_formatting#Sous_je_okma_C3'AEnes)
- [Documentation complète de la méthode `.slice()`](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/slice)
- [Documentation complète de la syntaxe d'index de tableau](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String#Create_a_string_using_ES6_format)