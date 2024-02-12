---
title:                "Utilisation des expressions régulières"
aliases:
- fr/typescript/using-regular-expressions.md
date:                  2024-02-03T19:19:05.333545-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilisation des expressions régulières"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/using-regular-expressions.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Les expressions régulières, ou regex, sont un outil puissant de correspondance et de recherche de motifs en programmation. Les programmeurs utilisent regex pour des tâches comme valider une entrée utilisateur, rechercher dans un texte ou manipuler des chaînes de caractères car c'est efficace et polyvalent.

## Comment faire :

Plongeons dans TypeScript pour voir comment les regex sont utilisées pour des tâches courantes.

```TypeScript
// Définir un motif regex pour une adresse e-mail
const emailPattern = /\S+@\S+\.\S+/;

// Tester si une chaîne correspond au motif de l'e-mail
const email = "user@example.com";
console.log(emailPattern.test(email)); // Sortie : true

// Trouver et remplacer les chiffres dans une chaîne
const replaceDigits = "Item 25 costs $30".replace(/\d+/g, '#');
console.log(replaceDigits); // Sortie : "Item # costs $#"

// Extraire des parties spécifiques d'une chaîne en utilisant des groupes de capture
const data = "April 10, 2021";
const datePattern = /(\w+) (\d+), (\d+)/;
const [, mois, jour, an] = datePattern.exec(data) || [];
console.log(mois, jour, an); // Sortie : "April" "10" "2021"
```

## Approfondissement

Dans les années 1950, le mathématicien Stephen Kleene a décrit les expressions régulières comme un modèle pour représenter les langages réguliers, ce qui est devenu essentiel en informatique. Plus tard, les regex sont devenues omniprésentes en programmation pour traiter le texte.

Bien que regex soit un couteau suisse pour les opérations sur les chaînes, cela n'est pas sans alternatives. Selon la complexité de la tâche, parfois des méthodes de chaîne comme `includes()`, `startsWith()`, `endsWith()`, ou même l'analyse avec une bibliothèque peuvent être meilleures. Par exemple, analyser une chaîne JSON complexe en utilisant regex peut être un cauchemar — utilisez un analyseur JSON à la place.

En ce qui concerne la mise en œuvre, les regex en JavaScript et TypeScript sont basées sur la spécification du langage ECMAScript. Sous le capot, les moteurs utilisent des machines à états pour correspondre efficacement aux motifs. Il convient de noter que les opérations regex peuvent devenir coûteuses en termes de performance, particulièrement avec des motifs mal écrits — méfiez-vous du "catastrophic backtracking".

## Voir également

- MDN Web Docs sur les expressions régulières : [MDN Expressions régulières](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions)
- Regex101 : Un outil pour tester et déboguer les motifs regex [Regex101](https://regex101.com/)
- Le livre "Maîtriser les expressions régulières" pour une compréhension approfondie : [O'Reilly](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)
