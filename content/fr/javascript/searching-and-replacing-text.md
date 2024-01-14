---
title:                "Javascript: Recherche et remplacement de texte"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Pourquoi

La recherche et le remplacement de texte sont des tâches courantes en programmation et peuvent aider à gagner du temps et à améliorer l'efficacité du code. Cela peut être particulièrement utile lors de la mise à jour de plusieurs occurrences de texte dans un projet.

## Comment faire

La recherche et le remplacement de texte en Javascript peuvent être réalisés de plusieurs manières. Voici deux méthodes couramment utilisées :

- Utilisation de la méthode replace() : Cette méthode prend en paramètres le texte à remplacer ainsi que le nouveau texte à insérer et retourne un nouveau string avec les modifications apportées. Voici un exemple de code :

```Javascript
let phrase = "Je suis un développeur Javascript.";
let nouvellePhrase = phrase.replace("Javascript", "Python");
console.log(nouvellePhrase); // Output : Je suis un développeur Python.
```

- Utilisation d'expressions régulières : Les expressions régulières sont une puissante méthode pour trouver et remplacer des patterns spécifiques dans une chaîne de caractères. Voici un exemple utilisant l'opérateur de remplacement global "g" pour remplacer toutes les occurrences d'une lettre spécifique :

```Javascript
let phrase = "Je suis un fan de Javascript.";
let nouvellePhrase = phrase.replace(/a/g, "e");
console.log(nouvellePhrase); // Output : Je suis un fen de Jevescript.
```

## Plongée en profondeur

En plus des méthodes mentionnées ci-dessus, il existe d'autres façons de rechercher et de remplacer du texte en Javascript. Il est également possible de combiner des expressions régulières avec des fonctions pour une plus grande flexibilité, ou d'utiliser des librairies externes telles que "lodash" pour faciliter ces tâches.

Il est important de noter que la méthode replace() ne modifie pas la chaîne de caractères d'origine, mais retourne un nouveau string avec les modifications. Pour modifier directement la chaîne, il faut utiliser des fonctions telles que substring() ou splice().

## Voir aussi

- [Documentation MDN de la méthode replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Guide de référence regex en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_régulières)
- [Documentation lodash](https://lodash.com/docs/4.17.15)