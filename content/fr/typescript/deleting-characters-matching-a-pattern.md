---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Supprimer des caractères correspondant à un motif en TypeScript

## Qu'est-ce que c'est et Pourquoi?

Supprimer des caractères correspondant à un motif signifie identifier et supprimer tous les caractères dans une chaîne qui correspondent à un certain modèle. C'est utile pour nettoyer les données, comme enlever les espaces supplémentaires ou les caractères spéciaux inutiles.

## Comment faire:

Voici un exemple simple d'utilisation de `replace()` avec une expression régulière pour supprimer tous les espaces d'une chaîne en TypeScript:

```TypeScript
let str = "Bonjour, Comment ça va?";
let nouvelleChaine = str.replace(/\s/g, '');
console.log(nouvelleChaine);
```

Sortie:

```TypeScript
"Bonjour,Commentçava?"
```

## Plongée en profondeur

De base, TypeScript ne propose pas de fonction pour supprimer des caractères spécifiques directement. On laisse habituellement ce travail aux expressions régulières, comme illustré. Historiquement, les expressions régulières sont un outil puissant pour manipuler les chaînes de caractères qui ont leur origine dans les langages de script Unix dans les années 70.

Il est également possible d'atteindre le même objectif en utilisant une boucle pour parcourir chaque caractère et construire une nouvelle chaîne, mais cette méthode est souvent beaucoup plus lente et verbeuse.

En termes de mise en œuvre, la méthode `replace()` avec une expression régulière utilise un automate fini déterministe sous le capot pour trouver et remplacer les correspondances, ce qui lui confère sa rapidité.

## Voir aussi

- Pour en savoir plus sur `replace()`: [MDN Web Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- Pour en savoir plus sur les expressions régulières: [Guide des expressions régulières JavaScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Expressions_régulières)
- Pour en savoir plus sur l'histoire des expressions régulières: [Histoire des expressions régulières](https://fr.wikipedia.org/wiki/Expression_régulière#Histoire)