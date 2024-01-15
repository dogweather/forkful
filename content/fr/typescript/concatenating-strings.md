---
title:                "Concaténer des chaînes de caractères"
html_title:           "TypeScript: Concaténer des chaînes de caractères"
simple_title:         "Concaténer des chaînes de caractères"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Pourquoi
Les chaînes de caractères jouent un rôle essentiel dans la manipulation des données en programmation, notamment pour l'affichage d'informations à l'utilisateur. La concaténation de chaînes de caractères permet de fusionner plusieurs chaînes en une seule, offrant ainsi une flexibilité et une facilité accrues dans la construction de messages ou d'ensembles de données.

## Comment faire
La concaténation de chaînes de caractères en TypeScript est facile à réaliser à l'aide de l'opérateur "+" ou en utilisant la méthode "concat()". Voici un exemple de code pour concaténer deux chaînes de caractères et afficher le résultat dans la console :

```TypeScript
let nom = "Jean";
let age = 25;
console.log("Salut, je m'appelle " + nom + " et j'ai " + age + " ans.");
```
```bash
Salut, je m'appelle Jean et j'ai 25 ans.
```
On peut également utiliser l'opérateur "+=" pour concaténer une chaîne à une autre :

```TypeScript
let message = "Bonjour";
message += " tout le monde!";
console.log(message);
```
```bash
Bonjour tout le monde!
```

## Plongée en profondeur
En plus de l'opérateur "+" et de la méthode "concat()", il existe d'autres façons de concaténer des chaînes de caractères en TypeScript telles que l'utilisation du backtick (`` ` ``) pour créer des interpolations de chaînes de caractères. Par exemple :

```TypeScript
let nom = "Marie";
let age = 30;
console.log(`Salut, je m'appelle ${nom} et j'ai ${age} ans.`);
```
```bash
Salut, je m'appelle Marie et j'ai 30 ans.
```
On peut également utiliser la méthode "join()" pour concaténer plusieurs chaînes en une seule, en spécifiant le séparateur souhaité :

```TypeScript
let fruits = ["pomme", "banane", "orange"];
let listeFruits = fruits.join(" | ");
console.log(listeFruits);
```
```bash
pomme | banane | orange
```

## Voir aussi
- [Documentation officielle de TypeScript sur la concaténation de chaînes](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html#string-concatenation)
- [Guide de référence pour les chaînes de caractères en TypeScript](https://www.typescriptlang.org/docs/handbook/2/strings.html#string-concatenation)