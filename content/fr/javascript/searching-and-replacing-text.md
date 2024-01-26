---
title:                "Recherche et remplacement de texte"
date:                  2024-01-20T17:58:02.948038-07:00
model:                 gpt-4-1106-preview
simple_title:         "Recherche et remplacement de texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
La recherche et le remplacement de texte, c'est changer une chaîne de caractères par une autre dans une donnée. Les programmeurs font ça pour corriger des erreurs, mettre à jour des informations ou manipuler des données.

## How to:
Chercher et remplacer avec JavaScript, c'est facile. Voici comment:

```javascript
let phrase = "Bonjour monde!";
let nouvellePhrase = phrase.replace("monde", "univers");
console.log(nouvellePhrase); // Affiche "Bonjour univers!"
```

Avec les expressions régulières, on peut être plus malin:

```javascript
let texte = "Les 7 merveilles du monde";
let texteModifie = texte.replace(/[0-9]+/, "8");
console.log(texteModifie); // Affiche "Les 8 merveilles du monde"
```

## Deep Dive
Rechercher et remplacer du texte en JavaScript utilise principalement la méthode `.replace()`. Historiquement, cette opération est fondamentale pour le traitement de texte dans les langages de programmation, car elle permet des modifications en masse. En dehors de `.replace()`, on a aussi des méthodes comme `.replaceAll()` ou des bibliothèques comme `lodash` pour plus de flexibilité.

La particularité de JavaScript, c'est de pouvoir utiliser des expressions régulières pour des recherches complexes. Une expression régulière est une séquence de caractères qui forme un modèle de recherche. Par exemple, pour remplacer chaque espace par un tiret:

```javascript
let texteUtilisateur = "Espace, la frontière finale";
let urlAmicale = texteUtilisateur.replace(/ /g, "-");
console.log(urlAmicale); // Affiche "Espace-la-frontière-finale"
```

Attention : `.replace()` ne modifie pas la chaîne originale, elle renvoie une nouvelle chaîne.

## See Also
Pour aller plus loin avec les expressions régulières en JavaScript :

- MDN Web Docs sur `.replace()` : [MDN String.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- Guide des expressions régulières : [MDN Regular Expressions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- Utilisation avancée et exemples : [Eloquent JavaScript - Regular Expressions](https://eloquentjavascript.net/09_regexp.html)
