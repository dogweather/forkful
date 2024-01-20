---
title:                "Suppression de caractères correspondant à un motif"
html_title:           "C: Suppression de caractères correspondant à un motif"
simple_title:         "Suppression de caractères correspondant à un motif"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/javascript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

# Supprimer des Caractères Correspondant à un Motif en Javascript

## Quoi & Pourquoi ? 

Supprimer des caractères correspondant à un motif consiste à éliminer des lettres ou des chiffres d'une chaîne de caractères en fonction d'une règle définie, appelée "motif". Nous faisons cela pour nettoyer nos données, en supprimer les aspects inutiles, ou manipuler des chaînes de caractères efficacement.

## Comment faire :

Voici comment vous pouvez supprimer tous les chiffres d'une chaîne de caractères en JavaScript :

```Javascript 
var maChaine = "J'adore le Javascript 24/7 !";
var motif = /\d+/g; 
var nouvelleChaine = maChaine.replace(motif, '');
console.log(nouvelleChaine); // "J'adore le Javascript !"
```
Dans ce code, `/\d+/g` est un motif qui correspond à tous les chiffres. `replace(motif, '')` supprime tous les chiffres de la chaîne.

## Exploration Approfondie 

Supprimer des caractères correspondant à un motif est une technique qui existe depuis l'époque des premiers langages de programmation. En JavaScript, la méthode `replace()` est la manière la plus courante d'y parvenir, mais il en existe d'autres. Par exemple, vous pouvez utiliser une boucle `for` pour parcourir la chaîne de caractères, bien que cette méthode ne soit pas très efficace.

La méthode `replace()` utilise une expression régulière (le "motif") pour identifier les caractères à supprimer. Les motifs de votre expression régulière doivent être soigneusement écrits pour éviter les erreurs de suppression.

## Voir Aussi 

Pour plus d'informations sur les expressions régulières en JavaScript, consultez [MDN Web Docs](https://developer.mozilla.org/fr/docs/Web/JavaScript/Guide/Regular_Expressions).

Pour la documentation de la méthode `replace()`, consultez [ce lien](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace).