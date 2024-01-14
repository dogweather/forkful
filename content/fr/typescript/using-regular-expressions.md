---
title:    "TypeScript: Utiliser des expressions régulières"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Pourquoi utiliser les expressions régulières?

Les expressions régulières sont des outils puissants pour la manipulation de texte dans les programmes TypeScript. Elles permettent de rechercher et de manipuler des motifs spécifiques dans une chaîne de caractères, ce qui peut être très utile pour la validation de données, la recherche de mots-clés ou la modification de chaînes de caractères. L'incorporation de ces expressions régulières dans votre code peut grandement améliorer l'efficacité et la précision de vos opérations de traitement de texte.

## Comment les utiliser?

Pour utiliser les expressions régulières en TypeScript, vous devez d'abord créer une instance de l'objet `RegExp`. Cette instance peut être créée avec un motif de recherche entre deux barres obliques, comme `/pattern/`, ou en utilisant le constructeur `new RegExp("pattern")`.

Une fois que vous avez votre instance `RegExp`, vous pouvez l'utiliser pour effectuer diverses opérations sur une chaîne de caractères, telles que la recherche de correspondances avec la méthode `test()` ou le remplacement de certaines parties avec la méthode `replace()`. Voici un exemple de code pour rechercher si une chaîne de caractères contient une adresse e-mail valide :

```TypeScript
let emailPattern = /^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$/;
let input = "example@email.com";
let result = emailPattern.test(input);
console.log(result); // output: true
```

Vous pouvez également utiliser des expressions régulières avec les méthodes intégrées de TypeScript telles que `split()` et `match()` pour diviser une chaîne de caractères ou extraire des informations spécifiques.

## Plongée en profondeur

Les expressions régulières permettent également d'utiliser des modificateurs pour effectuer des recherches plus avancées. Par exemple, le modificateur `g` permet de trouver toutes les occurrences d'un motif dans une chaîne de caractères, tandis que `i` ignore la casse. Vous pouvez également utiliser des groupes de capture pour extraire spécifiquement des parties de la chaîne correspondant à certains motifs.

Il est important de noter que les expressions régulières peuvent parfois être complexes et difficiles à comprendre. Il existe également différentes variantes de syntaxe selon le langage de programmation. Il est donc recommandé de lire attentivement la documentation officielle de TypeScript sur les expressions régulières avant de les utiliser.

## Voir aussi

- [Documentation officielle de TypeScript sur les expressions régulières](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Tutoriel sur les expressions régulières en TypeScript](https://www.digitalocean.com/community/tutorials/js-regex-updated)
- [Outil en ligne pour tester des expressions régulières](https://www.regextester.com/typeScript)

---

## Liens en français

- [Guide complet pour débuter en TypeScript](https://www.grafikart.fr/tutoriels/typescript) (par Grafikart)
- [Programmer en TypeScript, la maîtrise](https://www.talkeezy.com/fr/training-xebia/ts/2/typescript-la-maitrise) (par Xebia France)
- [Développer en TypeScript avec Angular](https://www.formationjavascript.com/programmation-typescript-debutant) (par Formation JavaScript)