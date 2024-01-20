---
title:                "Interpolation d'une chaîne de caractères"
html_title:           "Ruby: Interpolation d'une chaîne de caractères"
simple_title:         "Interpolation d'une chaîne de caractères"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

# Interpolation de chaîne en Javascript 

## Qu'est-ce et pourquoi ?

L'interpolation de chaîne est une manière de créer et de manipuler des chaînes de caractères, en insérant des variables directement dans la chaîne. Cela simplifie l'opération d'assemblage de chaînes et rend le code plus lisible.

## Comment faire :

L'interpolation de chaînes est possible grâce aux littéraux de gabarits ES6. Voici à quoi ils ressemblent:

```Javascript
let nom = "Sarah";
let age = 30;

// Utilisation de l'interpolation de chaîne
let message = `Bonjour, je suis ${nom} et j'ai ${age} ans.`;

console.log(message);
// Affiche : "Bonjour, je suis Sarah et j'ai 30 ans."
```
Ici, `${nom}` et `${age}` sont remplacées par les valeurs des variables `nom` et `age`.

## Plongée en profondeur :

L'interpolation de chaîne est une caractéristique introduite dans ES6 (ECMAScript 2015). Avant cela, on devait utiliser l'opérateur de concaténation (`+`) pour assembler des chaînes de caractères, ce qui pouvait être verbeux et difficile à lire.

```Javascript
let message = "Bonjour, je suis " + nom + " et j'ai " + age + " ans.";
```

En plus de l'interpolation de chaîne, il existe des méthodes alternatives comme `concat()` ou `join()`, mais elles sont généralement plus lourdes.

Lorsqu'on utilise l'interpolation, il faut se rappeler que la substitution dans la chaîne suit les règles de conversion de type de JavaScript. Par conséquent, la valeur insérée est convertie en une chaîne si elle ne l'est déjà.

## Voir également :

- [String Interpolation in JavaScript](https://ui.dev/template-literals/) - Article exhaustif sur l'interpolation de chaîne.
- [Template literals (Template strings) sur MDN](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Template_literals) - Documentation complète de Mozilla.
- [ES6 Template Literals, the Handlebars killer?](http://2ality.com/2015/01/template-strings-html.html) - Comparaison entre les littéraux de gabarits et Handlebars.