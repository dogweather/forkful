---
title:                "TypeScript: Convertir une date en chaîne de caractères"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/fr/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Pourquoi

Il y a plusieurs raisons pour lesquelles un programmeur peut être amené à convertir une date en chaîne de caractères. Cela peut être utile pour afficher la date dans un format spécifique, pour faciliter les comparaisons de dates ou pour stocker la date sous forme de chaîne dans une base de données.

## Comment faire

Pour convertir une date en chaîne de caractères en TypeScript, vous pouvez utiliser la méthode `toLocaleString()` de l'objet `Date`. Elle prend en paramètres les options de formatage tels que la langue, le fuseau horaire et le format de date et renvoie une chaîne contenant la date formatée.

Voici un exemple de code qui convertit la date courante en chaîne dans le format "jj/mm/aaaa" (jour/mois/année) en utilisant la langue française :

```TypeScript
let date = new Date();
let options = { day: 'numeric', month: 'numeric', year: 'numeric', timeZone: 'UTC' };
let dateAsStr = date.toLocaleString('fr-FR', options);
console.log(dateAsStr); // output: 01/06/2021
```

Il est également possible de spécifier un format de date personnalisé en utilisant les options `weekday`, `day`, `month` et `year` avec le motif de formatage souhaité. Par exemple, pour obtenir une date formatée en "mardi 1er juin 2021", vous pouvez utiliser les options suivantes :

```TypeScript
let options = { weekday: 'long', day: 'numeric', month: 'long', year: 'numeric' };
```

Il est important de noter que la méthode `toLocaleString()` renvoie le résultat de la conversion basé sur les paramètres régionaux de l'ordinateur, donc le format peut varier en fonction de l'endroit où le code est en train d'être exécuté. Il est recommandé d'utiliser l'option `timeZone` pour assurer une conversion de date cohérente.

## Plongée en profondeur

La méthode `toLocaleString()` utilise les fonctions internes de `Date` pour formater la date et l'heure, alors il est utile de connaître ces fonctions pour comprendre comment la conversion en chaîne de caractères se fait. Par exemple, la fonction `getDay()` renvoie le jour de la semaine en utilisant un système de numérotation 0-6 (0 pour dimanche, 6 pour samedi), la fonction `getDate()` renvoie le jour du mois et la fonction `getMonth()` renvoie le mois de l'année en utilisant un système de numérotation 0-11.

En utilisant ces fonctions et en utilisant des méthodes de manipulation de chaînes comme `padStart()` ou `slice()`, il est possible de construire un format de date personnalisé à partir de la date et de l'heure. Vous pouvez également importer des packages tels que Moment.js pour un formatage de date plus avancé.

## Voir aussi

- [Documentation sur la méthode `toLocaleString()` en TypeScript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString)
- [Guide sur les options de formatage de dates en TypeScript](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [Site officiel de Moment.js](https://momentjs.com/)