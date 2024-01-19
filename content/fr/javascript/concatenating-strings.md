---
title:                "Concaténation de chaînes"
html_title:           "C: Concaténation de chaînes"
simple_title:         "Concaténation de chaînes"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Qu'est-ce que & Pourquoi ?
La concaténation des chaînes de caractères désigne le processus d'union de deux chaînes ou plus en une seule. Les programmeurs l'utilise généralement pour combiner des informations textuelles.

## Comment le faire :
Vous avez plusieurs façons de concaténer des chaînes en JavaScript. 

```Javascript
// Méthode 1 : Opérateur +
let chaine1 = 'Bonjour';
let chaine2 = ', le monde!';
let resultat = chaine1 + chaine2;
console.log(resultat); // 'Bonjour, le monde!'

// Méthode 2 : Méthode concat()
let chaine3 = 'Comment';
let chaine4 = ' ça va?';
let resultat2 = chaine3.concat(chaine4);
console.log(resultat2); // 'Comment ça va?'

// Méthode 3 : Template literals
let chaine5 = 'JavaScript';
let chaine6 = ' est génial !';
let resultat3 = `${chaine5}${chaine6}`;
console.log(resultat3); // 'JavaScript est génial !'
```

## Plongée plus profonde :
**Contexte historique**
Historiquement, en JavaScript, l'opérateur `+` est devenu la solution de concaténation par défaut.

**Alternatives**
Depuis l'avènement d'ES6 en 2015, les littéraux de gabarit offrent une approche plus moderne : ils gèrent bien l'espacement et facilitent l'incorporation de variables.

**Détails d'implémentation**
Regardez la mémoire et les performances. Si vous travaillez avec de grandes chaînes, `.concat()` peut accélérer votre code. Cependant, pour des opérations courantes, l'opérateur `+` ou les littéraux de modèle fonctionnent très bien.

## Voir aussi :
- [MDN Web Docs: Chaînes de caractères](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String)
- [MDN Web Docs: Littéraux de gabarit](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Litt%C3%A9raux_gabarits)
- [ExploringJS: JavaScript for impatient programmers](http://exploringjs.com/impatient-js/)