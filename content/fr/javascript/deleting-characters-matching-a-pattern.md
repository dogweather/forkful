---
title:    "Javascript: Suppression de caractères correspondant à un motif."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

Supprimer des caractères correspondant à un motif peut sembler une tâche banale, mais elle peut être très utile dans certaines situations. Par exemple, lorsque vous travaillez avec des données textuelles, vous pouvez avoir besoin de nettoyer les chaînes de caractères en supprimant certaines parties qui correspondent à un motif précis. Cela peut vous aider à simplifier et à organiser vos données, économisant ainsi du temps et des efforts lors du traitement de celles-ci.

## Comment faire 

Pour supprimer des caractères correspondant à un motif, vous pouvez utiliser la méthode `replace()` en combinaison avec des expressions régulières (regex) en JavaScript. Voici un exemple de code pour supprimer toutes les voyelles d'une chaîne de caractères :

```Javascript
let string = "Bonjour!"
let filteredString = string.replace(/[aeiou]/gi, "")
console.log(filteredString) // Bnjr!
```

Dans cet exemple, nous utilisons une regex entre les `/` qui indique tous les caractères à supprimer (ici les voyelles) et les options `g` et `i` pour qu'elle recherche toutes les occurrences et qu'elle ignore la casse. Nous passons ensuite cette regex en premier argument de la méthode `replace()` et la chaîne de caractères à nettoyer en deuxième argument. La méthode renvoie une nouvelle chaîne de caractères avec les caractères correspondant au motif supprimés.

## Plongée en profondeur

La méthode `replace()` en JavaScript peut être utilisée de différentes manières pour supprimer des caractères correspondants à un pattern. Par exemple, vous pouvez utiliser une fonction de substitution en deuxième argument pour remplacer les caractères correspondants par quelque chose d'autre ou même passer une fonction en premier argument pour effectuer des opérations plus complexes sur la partie correspondante de la chaîne.

Il est également possible de combiner plusieurs expressions régulières pour supprimer plusieurs motifs à la fois ou utiliser des modificateurs pour effectuer des opérations plus précises (par exemple, uniquement sur le début ou la fin de la chaîne).

En apprenant à utiliser les expressions régulières et la méthode `replace()` en JavaScript, vous pourrez effectuer des opérations de nettoyage de données plus efficaces et plus précises.

## Voir aussi

- [Documentation officielle de la méthode replace()](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/String/replace)
- [Guide pour apprendre les expressions régulières en JavaScript](https://flaviocopes.com/javascript-regular-expressions/)
- [Différents modificateurs pour les expressions régulières en JavaScript](https://www.w3schools.com/jsref/jsref_obj_regexp.asp)