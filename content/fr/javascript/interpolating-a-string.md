---
title:                "Interpoler une chaîne de caractères"
html_title:           "Javascript: Interpoler une chaîne de caractères"
simple_title:         "Interpoler une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Qu'est-ce que c'est et pourquoi?

L'interpolation de chaîne est une méthode en programmation Javascript qui permet d'insérer des variables ou des expressions dans une chaîne de caractères. Cela rend le code plus lisible et permet d'éviter de concaténer plusieurs chaînes pour obtenir un résultat souhaité. Les programmeurs utilisent cette méthode pour faciliter la manipulation de données et rendre leur code plus efficace.

## Comment faire:

```Javascript
// Définir une variable
let nom = "Marc";
// Utiliser la méthode d'interpolation pour insérer la variable dans une chaîne
console.log(`Bonjour ${nom} !`);
```

Résultat: Bonjour Marc !

## Profonde immersion:

Historiquement, l'interpolation de chaîne est apparue dans les langages de programmation comme Perl et PHP avant d'être intégrée dans ECMAScript 6 en 2015. D'autres alternatives à l'interpolation de chaîne incluent l'utilisation de la méthode `concat()` ou l'opérateur `+`. Cependant, ces méthodes peuvent rapidement devenir fastidieuses lorsqu'il s'agit de concaténer plusieurs variables ou expressions.

En termes d'implémentation, l'interpolation de chaîne utilise l'opérateur `+` pour concaténer une chaîne et une expression, et le symbole ` ` pour délimiter les variables à insérer.

## Voir aussi:

Pour plus d'informations sur l'interpolation de chaîne en Javascript, consultez ces sources:

- [La documentation officielle de MDN sur l'interpolation de chaîne](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Litt%C3%A9raux_templates)
- [Un tutoriel sur l'interpolation de chaîne en Javascript](https://www.sitepoint.com/string-interpolation-in-javascript/)
- [Une explication en détail de l'interpolation de chaîne en ECMAScript 6](https://medium.com/javascript-in-plain-english/string-interpolation-in-monitor-in-ecmascript-6-d24de8ce63fe)