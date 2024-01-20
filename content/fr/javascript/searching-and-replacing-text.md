---
title:                "Recherche et remplacement de texte"
html_title:           "Arduino: Recherche et remplacement de texte"
simple_title:         "Recherche et remplacement de texte"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## C'est Quoi & Pourquoi ?
Rechercher et remplacer du texte consiste à identifier une séquence spécifique dans une chaîne de caractères et à la substituer par une autre. Les programmeurs le font pour manipuler et traiter les données textuelles selon les besoins de l'application.

## Comment faire :
Pour rechercher et remplacer du texte en Javascript, nous utilisons la méthode `replace()`. Voyez l'exemple ci-dessous :

```Javascript
let phrase = "J'aime le Javascript.";
let nouvellePhrase = phrase.replace('Javascript', 'programmation');
console.log(nouvellePhrase); // "J'aime la programmation."
```
Dans cet exemple, nous avons remplacé 'Javascript' par 'programmation'.

## Approfondissement :
Historiquement, la recherche et le remplacement de texte est une partie importante de la manipulation de chaînes de caractères dans tous les langages de programmation. En Javascript, `replace()` est une fonction intégrée qui simplifie cette tâche.

Il existe des alternatives à `replace()`, comme l'utilisation de l'expression régulière pour remplacer plusieurs occurrences en une seule fois :

```Javascript
let phrase = "J'adore Javascript. Javascript c'est super!";
let nouvellePhrase = phrase.replace(/Javascript/g, 'programmation');
console.log(nouvellePhrase); // "J'adore programmation. Programmation c'est super!"
```
Dans cette exemple, `/Javascript/g` est une expression régulière qui cherche globalement 'Javascript' dans la chaîne.

Quant aux détails d'implémentation de `replace()`, il est important de noter que cette méthode ne modifie pas la chaîne originale, mais renvoie une nouvelle chaîne.

## Voir Aussi :
Pour plus d'informations, consultez les liens suivants :
- [MDN Web Docs : String.prototype.replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Javascript.info : Recherche de motifs avec des drapeaux](https://javascript.info/regexp-introduction)
- [Eloquent JavaScript : Expressions Régulières](https://eloquentjavascript.net/09_regexp.html)