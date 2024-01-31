---
title:                "Organisation du code en fonctions"
date:                  2024-01-26T01:12:03.035885-07:00
model:                 gpt-4-1106-preview
simple_title:         "Organisation du code en fonctions"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Organiser le code en fonctions signifie découper votre code en blocs réutilisables et modulaires. Nous faisons cela pour respecter le principe DRY (Don't Repeat Yourself, ou Ne vous répétez pas), cela rend le code plus clair, plus facile à lire et simple à déboguer.

## Comment faire :
Imaginez que vous créez une calculatrice de base. Au lieu d'écrire la logique d'addition partout où vous en avez besoin, créez une fonction `add` :

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // Exemple de sortie : 12
```

Maintenant, disons que nous avons besoin d'une fonction pour multiplier :

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // Exemple de sortie : 12
```
Remarquez comment nous nous concentrons sur une tâche par fonction ? C'est le cœur de l'organisation du code.

## Plongée en Profondeur
Historiquement, à mesure que les langages de programmation ont évolué, les fonctions sont devenues essentielles dans la structuration du code, s’inspirant des fonctions mathématiques. Elles sont un pilier de la programmation procédurale et perdurent dans les paradigmes de programmation orientée objet et fonctionnelle.

Des alternatives ? Vous pourriez simplement ne pas utiliser de fonctions, mais c'est un aller simple pour Spaghetti Town. Ou alors opter pour la POO (Programmation Orientée Objet) et emballer la fonctionnalité dans des méthodes — qui sont en gros des fonctions appartenant à des objets.

En terme d'implémentation, TypeScript insiste sur les types. Définir les types d'entrée et de sortie pour les fonctions n'est pas seulement une question de bonnes manières ; c'est un must pour un code TypeScript propre. De plus, avec TypeScript, vous bénéficiez de fonctionnalités intéressantes telles que les surcharges, les génériques et les paramètres optionnels pour booster vos fonctions.

## Voir Aussi
Consultez ces ressources pour améliorer votre jeu de fonctions :

- [Manuel TypeScript – Fonctions](https://www.typescriptlang.org/docs/handbook/2/functions.html) : Votre bible pour les fonctions TypeScript.
- [Code Propre JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions) : Appliquez les principes du Code Propre à vos fonctions JavaScript.
- [You Don’t Know JS – Portée & Fermetures](https://github.com/getify/You-Dont-Know-JS) : Maîtrisez le fonctionnement des fonctions avec la portée et les fermetures en JavaScript.
