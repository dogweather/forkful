---
title:    "Javascript: Comparaison de deux dates"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Pourquoi

La comparaison de deux dates est une tâche courante en programmation Javascript. Cela peut être utile pour vérifier si une date est antérieure ou postérieure à une autre, ou pour calculer la différence entre deux dates. Cet article vous expliquera comment effectuer une telle comparaison en utilisant Javascript.

## Comment faire

La première étape pour comparer deux dates en Javascript est de créer des objets Date pour chaque date que vous souhaitez comparer. Vous pouvez le faire en utilisant la syntaxe suivante :

```Javascript
const date1 = new Date('2020-06-15');
const date2 = new Date('2020-06-20');
```

Une fois que vous avez créé vos objets Date, vous pouvez utiliser les méthodes intégrées pour les comparer. Voici quelques exemples :

- Pour vérifier si une date est antérieure à une autre, vous pouvez utiliser la méthode `.getTime()` pour obtenir le nombre de millisecondes écoulées depuis le 1er janvier 1970, puis comparer ces valeurs :

```Javascript
if (date1.getTime() < date2.getTime()) {
  console.log('date1 est antérieure à date2');
}
```

- Pour vérifier si une date est postérieure à une autre, vous pouvez utiliser la même méthode, mais en inversant les valeurs :

```Javascript
if (date2.getTime() > date1.getTime()) {
  console.log('date2 est postérieure à date1');
}
```

- Pour calculer la différence en jours entre deux dates, vous pouvez utiliser la méthode `.getDate()` pour obtenir le jour du mois de chaque date, puis soustraire ces valeurs :

```Javascript
const differenceEnJours = date2.getDate() - date1.getDate();
console.log(`Il y a ${differenceEnJours} jours entre date1 et date2.`);
```

En utilisant ces méthodes, vous pouvez facilement comparer deux dates en Javascript et effectuer des actions en conséquence.

## Deep Dive

Il est important de noter que la comparaison de dates en Javascript peut être délicate en raison de la façon dont les objets Date sont créés. Par exemple, si vous utilisez la méthode `new Date()` sans arguments, cela créera un objet Date représentant la date et l'heure actuelles. Cela signifie que si vous exécutez votre code à des moments différents, vous obtiendrez des résultats différents pour les mêmes dates. Pour éviter cela, il est recommandé de spécifier une date précise lors de la création de votre objet Date.

De plus, la méthode `.getTime()` renvoie une valeur en millisecondes, ce qui signifie que si vous voulez comparer une date précise (par exemple le 1er novembre 2020 à 00h00) avec une autre date où l'heure est incluse (par exemple le 1er novembre 2020 à 15h00), vous devrez convertir cette dernière date en millisecondes en utilisant la méthode `.getTime()` et en soustrayant ensuite les valeurs.

Enfin, il existe plusieurs bibliothèques et modules disponibles pour faciliter la comparaison de dates en Javascript. N'hésitez pas à en explorer certaines pour trouver celle qui conviendra le mieux à vos besoins.

## Voir aussi

- [Documentation Mozilla sur l'objet Date en Javascript](https://developer.mozilla.org/fr/docs/Web/JavaScript/Reference/Objets_globaux/Date)
- [Comparaison de dates en Javascript avec Moment.js](https://momentjs.com/docs/#/displaying/difference/)
- [Comparaison de dates en Javascript avec Date-fns](https://date-fns.org/v2.14.0/docs/differenceInDays)