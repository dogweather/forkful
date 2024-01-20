---
title:                "Capitaliser une chaîne"
html_title:           "TypeScript: Capitaliser une chaîne"
simple_title:         "Capitaliser une chaîne"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Quoi & Pourquoi ?
Capitaliser une chaîne en TypeScript signifie transformer la première lettre de chaque mot d'une chaîne en majuscule. On le fait souvent pour donner une forme plus présentable à un texte ou à une entrée utilisateur.

## Comment faire :
Voici un exemple de code pour capitaliser une chaîne en TypeScript :

```TypeScript
function capitalize(str: string): string {
    return str.split(' ')
              .map(word => word.charAt(0).toUpperCase() + word.slice(1))
              .join(' ');
}

console.log(capitalize("bonjour le monde.")); // Output : "Bonjour Le Monde."
```
Le code ci-dessus divise la chaîne en mots, transforme la première lettre de chaque mot en majuscule, puis fusionne les mots pour former la chaîne finale.

## Plongée en Profondeur:
Historiquement, la capitalisation est un concept issu de l'anglais où les mots d'une phrase, d'un titre ou d'une entrée ont souvent leur première lettre en majuscule pour des raisons d'esthétique ou de lisibilité.

En TypeScript, une alternative à la fonction ci-dessus pourrait être d'utiliser une expression régulière à la place de `split()` et `map()`:

```TypeScript
function capitalizeRegex(str: string): string {
    return str.replace(/\b\w/g, l => l.toUpperCase());
}

console.log(capitalizeRegex("bonjour le monde.")); // Output : "Bonjour Le Monde."
```
Cette alternative peut être plus performante sur de grandes chaînes, mais peut aussi être plus difficile à comprendre pour les novices.

La méthode initiale divise d'abord la chaîne en mots en utilisant `split()`, puis utilise `map()` pour transformer la première lettre de chaque mot en majuscule avec `toUpperCase()`, et enfin `join()` pour fusionner les mots en une seule chaîne.

## Voir Aussi :
- [Méthode toUpperCase()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/toUpperCase)
- [Méthode split()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/String/split)
- [Méthode join()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Array/join)